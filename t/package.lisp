(defpackage #:erebus/test
  (:use :cl :fiveam :erebus)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:erebus/test)

(def-suite erebus
  :description "Erebus test suite")

;; The default *RANDOM-STATE* is identical in every fresh image, so
;; RANDOM-STRING (container names) and the random ports below would
;; otherwise repeat across separate test runs and collide in docker.
;; Seed it from OS entropy (/dev/urandom on SBCL), like MAIN does.
(setf *random-state* (make-random-state t))

(defun random-string (length)
  (funcall
   (gen-string :length (gen-integer :min length :max length)
               :elements (gen-character :code (gen-integer :min 97 :max 122)))))

;;; ---------------------------------------------------------------------------
;;; Docker container plumbing
;;;
;;; These helpers stand on their own so they can be driven both by the
;;; WITH-DOCKER-CONTAINER macro (automated tests) and by the DEV-* helpers
;;; further down (interactive, manual testing from the REPL).
;;; ---------------------------------------------------------------------------

(defun erebus-test-dir ()
  (merge-pathnames "t/" (asdf:system-source-directory :erebus/test)))

(defun junk-dir ()
  (merge-pathnames "junk/" (erebus-test-dir)))

(defun container-folder (name)
  (merge-pathnames (make-pathname :directory (list :relative name)) (junk-dir)))

(defun ensure-test-image ()
  "Build the ralt/erebus:latest docker image, but only when the Dockerfile
has changed since the last build (tracked with a stamp file)."
  (let* ((dir (erebus-test-dir))
         (dockerfile (probe-file (merge-pathnames "Dockerfile" dir)))
         (stamp-name (merge-pathnames ".git-ignore-me-container" dir))
         (stamp (probe-file stamp-name)))
    (when (or (not stamp)
              (> (file-write-date dockerfile) (file-write-date stamp)))
      (uiop:run-program (format nil "cd ~a && docker build -t ralt/erebus:latest ."
                                (namestring dir))
                        :output t :error-output t)
      ;; a quick version of "touch" that updates the mtime every run
      (close (open stamp-name :direction :output
                              :if-exists :supersede :if-does-not-exist :create)))))

(defun create-container (name folder vpn-local-port)
  (ensure-directories-exist folder)
  (uiop:run-program
   (format nil "docker create \\
                 --privileged \\
                 --publish ~a:1194/udp \\
                 --publish ~a:1194/tcp \\
                 --name ~a \\
                 --volume ~a:/etc/openvpn/ \\
                 --volume /lib/modules:/lib/modules \\
                 ralt/erebus:latest"
           vpn-local-port
           vpn-local-port
           name
           folder)
   :output t
   :error-output t))

(defun run-in-container (name command)
  (uiop:run-program (format nil "docker exec -i ~a bash -c ~s" name command)
                    :output t
                    :error-output t))

(defun run-in-container-output (name command)
  "Like RUN-IN-CONTAINER but returns the command's stdout as a (stripped)
string, for assertions."
  (uiop:run-program (format nil "docker exec -i ~a bash -c ~s" name command)
                    :output '(:string :stripped t)
                    :error-output t))

(defun run-python-in-container (name code)
  "Run CODE (a python3 program) inside container NAME and return its
stdout. The program is base64-encoded so we never have to fight shell
quoting for the (potentially multi-line, quote-heavy) source."
  (run-in-container-output
   name
   (format nil "echo ~a | base64 -d | python3"
           (cl-base64:string-to-base64-string code :columns 0))))

(defun prepare-container (name folder)
  (uiop:run-program
   (format nil "docker start ~a" name)
   :output t
   :error-output t)
  (copy-recursively (merge-pathnames "t/fixtures/openvpn/" (asdf:system-source-directory :erebus/test))
                    folder))

(defun copy-recursively (input output)
  (dolist (path (directory
                 (make-pathname :directory (pathname-directory input)
                                :name :wild
                                :type :wild)))
    (if (pathname-name path)
        (uiop:copy-file path (merge-pathnames (make-pathname :name (pathname-name path)
                                                             :type (pathname-type path))
                                              output))
        (let ((new-folder (merge-pathnames (make-pathname
                                            :directory (list
                                                        :relative
                                                        (first (last (pathname-directory path)))))
                                           output)))
          (ensure-directories-exist new-folder)
          (copy-recursively path new-folder)))))

(defun start-services (name)
  (run-in-container name "mkdir -p /run/nginx && nginx && nohup ovpn_run &"))

(defun restart-services (name)
  (run-in-container name "killall -9 nginx && killall -9 openvpn || true")
  (start-services name))

(defun cleanup-container (name folder)
  (uiop:run-program (format nil "docker rm --force ~a" name) :output t :error-output t)
  ;; because the folders are created as root inside the container, the
  ;; lisp process on the host will usually not have permissions to
  ;; delete it. we thus have to do the deletion inside the container
  ;; of most of the stuff, and then we can cleanup host-side.
  (uiop:run-program
   (format
    nil
    "docker run --rm -v ~a:/etc/openvpn -i ralt/erebus:latest bash -c 'rm -rf /etc/openvpn/*'"
    folder)
   :output t :error-output t)
  (uiop:delete-directory-tree folder :validate t))

;;; ---------------------------------------------------------------------------
;;; OpenVPN server configuration helpers
;;; ---------------------------------------------------------------------------

(defun openvpn-server-config (&key (proto "udp") (secret "secret static.key")
                                   (cipher "AES-256-CBC") (auth "SHA256") (verb 3))
  "Render an openvpn.conf for a static-key server. PROTO is \"udp\" or
\"tcp-server\". SECRET is the full secret directive, e.g. \"secret
static.key\" or \"secret static.key 0\"."
  (format nil "ifconfig 10.8.0.1 10.8.0.2
verb ~a
keepalive 10 60
persist-tun
~a
cipher ~a
auth ~a

proto ~a
port 1194
dev tun0
status /tmp/openvpn-status.log
log /etc/openvpn/openvpn.log
user nobody
group nogroup
comp-lzo no
"
          verb secret cipher auth proto))

(defun configure-openvpn (name &rest config-args &key pre &allow-other-keys)
  "Generate a fresh static key inside container NAME and write an
openvpn.conf there. PRE, when supplied, is a shell snippet run first (for
instance to launch a backend service). Remaining keyword args are passed
to OPENVPN-SERVER-CONFIG."
  (run-in-container
   name
   (format nil "~@[~a~%~]cd /etc/openvpn
openvpn --genkey --secret static.key
chmod 777 static.key
rm -rf ccd/ crl.pem pki/ # delete those or ovpn_run will try to use them

cat > /etc/openvpn/openvpn.conf <<'EOF'
~aEOF
"
           pre
           (apply #'openvpn-server-config (a:remove-from-plist config-args :pre)))))

(defun openvpn-prep-hook (&rest config-args)
  "Return a prepare-hook (a function of NAME and FOLDER) for
WITH-DOCKER-CONTAINER that configures the server with CONFIG-ARGS."
  (lambda (name folder)
    (declare (ignore folder))
    (apply #'configure-openvpn name config-args)))

;;; ---------------------------------------------------------------------------
;;; Erebus client / proxy helpers
;;; ---------------------------------------------------------------------------

(defun make-test-client (folder vpn-local-port &rest initargs)
  "Make an openvpn-client-static-key pointing at the server published on
VPN-LOCAL-PORT, using the static.key in FOLDER. INITARGS override the
defaults (e.g. :protocol :stream, :key-direction \"1\")."
  (apply #'make-instance 'openvpn-client-static-key
         (append initargs
                 (list :host "localhost"
                       :port vpn-local-port
                       :client-ip "10.8.0.2"
                       :secret (namestring
                                (make-pathname :name "static.key"
                                               :directory (pathname-directory folder)))
                       :cipher "AES-256-CBC"
                       :auth "SHA256"))))

(defmacro with-test-client ((client folder vpn-local-port &rest initargs) &body body)
  "Bind CLIENT to a connected test client and disconnect it afterwards."
  `(let ((,client (make-test-client ,folder ,vpn-local-port ,@initargs)))
     (connect ,client)
     (unwind-protect (progn ,@body)
       (disconnect ,client))))

(defun uint32-be (n)
  "N as a 4-byte big-endian octet vector."
  (make-array 4 :element-type '(unsigned-byte 8)
                :initial-contents (list (ldb (byte 8 24) n)
                                        (ldb (byte 8 16) n)
                                        (ldb (byte 8 8) n)
                                        (ldb (byte 8 0) n))))

(defun read-line-from-octet-stream (stream)
  "Read a newline-terminated ASCII line from a binary STREAM (without the
newline)."
  (with-output-to-string (out)
    (loop for b = (read-byte stream nil :eof)
          until (or (eq b :eof) (= b 10))
          do (write-char (code-char b) out))))

;;; A local (host-side) TCP server, used to test inbound port-forwarding:
;;; erebus exposes a VPN port and relays connections to one of these.

(defun %serve-local (listen-socket handler)
  (lambda ()
    (loop
      (let ((conn (usocket:socket-accept listen-socket :element-type '(unsigned-byte 8))))
        (bordeaux-threads:make-thread
         (lambda ()
           (unwind-protect
                (funcall handler (usocket:socket-stream conn))
             (ignore-errors (usocket:socket-close conn))))
         :name "local tcp connection")))))

(defmacro with-local-tcp-server ((port handler) &body body)
  "Start a host-side TCP server on 127.0.0.1 (ephemeral port bound to
PORT) that runs HANDLER, a (lambda (stream) ...), once per accepted
connection. Tear it down after BODY."
  (a:with-gensyms (listen thread)
    `(let* ((,listen (usocket:socket-listen "127.0.0.1" 0
                                            :element-type '(unsigned-byte 8)
                                            :reuse-address t
                                            ;; big enough to absorb a burst of
                                            ;; concurrent connections (the
                                            ;; inbound stress test) without the
                                            ;; OS dropping/resetting them.
                                            :backlog 128))
            (,port (usocket:get-local-port ,listen))
            (,thread (bordeaux-threads:make-thread (%serve-local ,listen ,handler)
                                                   :name "local tcp server")))
       (declare (ignorable ,port))
       (unwind-protect (progn ,@body)
         (ignore-errors (bordeaux-threads:destroy-thread ,thread))
         (ignore-errors (usocket:socket-close ,listen))))))

(defun read-n-octets (stream n)
  "Read exactly N octets from STREAM into a fresh vector."
  (let ((buffer (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    buffer))

(defun be32-to-integer (bytes)
  "Decode a 4-octet big-endian vector to an integer."
  (+ (ash (aref bytes 0) 24) (ash (aref bytes 1) 16)
     (ash (aref bytes 2) 8) (aref bytes 3)))

(defmacro with-proxy ((proxy-port client) &body body)
  "Start an erebus HTTP proxy bound to CLIENT on a random local port,
bind PROXY-PORT to it for the duration of BODY, and stop it afterwards."
  (a:with-gensyms (acceptor)
    `(let* ((,proxy-port (funcall (gen-integer :min 5000 :max 10000)))
            (,acceptor (make-instance 'acceptor
                                      :port ,proxy-port
                                      :address "127.0.0.1"
                                      :client ,client)))
       (hunchentoot:start ,acceptor)
       (unwind-protect (progn ,@body)
         (hunchentoot:stop ,acceptor)))))

;;; ---------------------------------------------------------------------------
;;; WITH-DOCKER-CONTAINER: spins a container up, runs BODY, tears it down
;;; ---------------------------------------------------------------------------

(defmacro with-docker-container ((container-name
                                  container-folder
                                  vpn-local-port
                                  &optional (prepare-hook (lambda (name folder)
                                                            (declare (ignore name folder)))))
                                 &body body)
  `(progn
     (ensure-test-image)
     (let* ((,container-name (format nil "erebus_~a" (random-string 20)))
            (,container-folder (container-folder ,container-name))
            (,vpn-local-port (funcall (gen-integer :min 10000 :max 60000))))
       ;; BODY may not use every binding (e.g. NAME); mark them ignorable
       ;; here so callers never need their own DECLARE.
       (declare (ignorable ,container-name ,container-folder ,vpn-local-port))
       (unwind-protect
            (progn
              (create-container ,container-name ,container-folder ,vpn-local-port)
              (prepare-container ,container-name ,container-folder)
              (funcall ,prepare-hook ,container-name ,container-folder)
              (start-services ,container-name)
              (progn ,@body))
         (cleanup-container ,container-name ,container-folder)))))

;;; ---------------------------------------------------------------------------
;;; Manual, interactive testing from the REPL
;;;
;;; Typical session:
;;;   (in-package :erebus/test)
;;;   (dev-vpn-up)                         ; or (dev-vpn-up :proto "tcp-server")
;;;   (defparameter *c (dev-client))       ; for :stream pass (dev-client :protocol :stream)
;;;   (defparameter *p (dev-proxy *c))     ; HTTP proxy on localhost:11023
;;;   ;; in a shell: http_proxy=http://localhost:11023 curl http://10.8.0.1
;;;   (hunchentoot:stop *p)
;;;   (disconnect *c)
;;;   (dev-vpn-down)
;;; ---------------------------------------------------------------------------

(defvar *dev-container* nil "Name of the running manual dev container, if any.")
(defvar *dev-folder* nil "Host config folder of the running manual dev container.")
(defvar *dev-port* nil "Local published VPN port of the running manual dev container.")

(defun dev-vpn-up (&rest config-args &key (name "erebus-dev") (port 11194) &allow-other-keys)
  "Bring up a long-running openvpn container for manual testing and leave
it running. Returns (values name folder port). CONFIG-ARGS are forwarded
to CONFIGURE-OPENVPN (e.g. :proto \"tcp-server\", :pre \"nohup echo-server &\")."
  (ensure-test-image)
  (let ((folder (container-folder name)))
    (create-container name folder port)
    (prepare-container name folder)
    (apply #'configure-openvpn name (a:remove-from-plist config-args :name :port))
    (start-services name)
    (setf *dev-container* name *dev-folder* folder *dev-port* port)
    (values name folder port)))

(defun dev-vpn-down (&optional (name *dev-container*))
  "Tear down the manual dev container brought up with DEV-VPN-UP."
  (when name
    (cleanup-container name (container-folder name))
    (setf *dev-container* nil *dev-folder* nil *dev-port* nil)))

(defun dev-client (&rest initargs)
  "Build and connect an erebus client against the running dev container.
Returns the connected client; remember to (disconnect ...) it."
  (let ((client (apply #'make-test-client *dev-folder* *dev-port* initargs)))
    (connect client)
    client))

(defun dev-proxy (client &key (port 11023))
  "Start an erebus HTTP proxy on PORT against CLIENT and return the
acceptor; (hunchentoot:stop ...) it when done."
  (let ((acceptor (make-instance 'acceptor :address "127.0.0.1" :port port :client client)))
    (hunchentoot:start acceptor)
    acceptor))

(defun dev-expose (client &key vpn-port (host "127.0.0.1") port)
  "Expose a local service to the dev VPN: forward inbound connections on
VPN-PORT (reachable at the client's VPN IP, 10.8.0.2) to HOST:PORT on this
machine. Returns the exposure handle; (unexpose ...) it when done.

For example, with a local web server on port 8080:
  (defparameter *e (dev-expose *c :vpn-port 8080 :port 8080))
  ;; from a shell:
  ;;   (run-in-container \"erebus-dev\" \"curl -s http://10.8.0.2:8080/\")
  (unexpose *e)"
  (expose client :vpn-port vpn-port :host host :port port))
