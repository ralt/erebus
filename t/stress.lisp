;;;; Stress / performance comparison for the erebus HTTP proxy.
;;;;
;;;; Fetches the *same* nginx resource from the host three ways and times
;;;; each, so the cost of erebus's userspace TCP-over-VPN path can be seen
;;;; next to a conventional proxy and a direct (no-proxy) baseline:
;;;;
;;;;   erebus    host -> erebus (userspace TCP) -> OpenVPN -> nginx
;;;;   tinyproxy host -> tinyproxy (OS sockets, in container) -> nginx
;;;;   direct    host -> nginx (no proxy)
;;;;
;;;; This is deliberately not apples-to-apples: only erebus crosses the
;;;; VPN and runs a userspace TCP stack. That difference *is* the cost
;;;; we are trying to measure.
;;;;
;;;; Run from the project root:
;;;;   sbcl --script t/stress.lisp
(load "~/quicklisp/setup.lisp")
(push (truename ".") asdf:*central-registry*)
(ql:quickload :erebus/test :verbose nil)
(in-package :erebus/test)

(defparameter *large-bytes* 262144 "Size of the throughput test file.")
(defparameter *large-iterations* 8)
(defparameter *small-iterations* 30)

(defun stress-create-container (name folder vpn-port nginx-port tiny-port)
  "Like CREATE-CONTAINER but also publishes nginx (80) and tinyproxy
(8888) to the host so they can be benchmarked from here."
  (ensure-directories-exist folder)
  (uiop:run-program
   (format nil "docker create --privileged \\
                 --publish ~a:1194/udp --publish ~a:1194/tcp \\
                 --publish ~a:80/tcp \\
                 --publish ~a:8888/tcp \\
                 --name ~a \\
                 --volume ~a:/etc/openvpn/ \\
                 --volume /lib/modules:/lib/modules \\
                 ralt/erebus:latest"
           vpn-port vpn-port nginx-port tiny-port name folder)
   :output t :error-output t))

(defun start-tinyproxy (name)
  ;; The User/Group directives are required: without them tinyproxy
  ;; daemonizes but never binds its port.
  (run-in-container
   name
   "cat > /etc/tinyproxy/tinyproxy.conf <<'EOF'
User tinyproxy
Group tinyproxy
Port 8888
Listen 0.0.0.0
Timeout 600
LogFile \"/var/log/tinyproxy/tinyproxy.log\"
LogLevel Info
MaxClients 100
StartServers 5
Allow 0.0.0.0/0
EOF
mkdir -p /var/log/tinyproxy && chown tinyproxy:tinyproxy /var/log/tinyproxy
tinyproxy -c /etc/tinyproxy/tinyproxy.conf"))

(defun fetch (url &key proxy)
  "Fetch URL (optionally through PROXY = (host port)), discarding but
counting the body. Returns the number of body bytes."
  (let ((body (drakma:http-request url :proxy proxy :close t
                                       :connection-timeout 30)))
    (length (if (stringp body) (babel:string-to-octets body) body))))

(defun bench (label iterations thunk)
  "Time ITERATIONS calls to THUNK (each returns a byte count) and print a
one-line summary."
  (let ((start (get-internal-real-time))
        (bytes 0))
    (dotimes (i iterations)
      (incf bytes (funcall thunk)))
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second 1.0)))
      (format t "~&  ~12a ~3d reqs  ~7,2fs   ~8,1f req/s   ~8,2f MB/s   ~9,2f ms/req~%"
              label iterations elapsed
              (/ iterations elapsed)
              (if (plusp bytes) (/ bytes elapsed 1024 1024) 0.0)
              (* 1000.0 (/ elapsed iterations)))
      elapsed)))

(let* ((name (format nil "erebus_stress_~a" (random-string 12)))
       (folder (container-folder name))
       (vpn-port (funcall (gen-integer :min 20000 :max 30000)))
       (nginx-port (funcall (gen-integer :min 30000 :max 40000)))
       (tiny-port (funcall (gen-integer :min 40000 :max 50000))))
  (handler-case
      (progn
        (ensure-test-image)
        (stress-create-container name folder vpn-port nginx-port tiny-port)
        (prepare-container name folder)
        (configure-openvpn
         name
         :pre (format nil "python3 -c \"open('/usr/share/man/bench.bin','wb').write(b'x'*~a)\""
                      *large-bytes*))
        (start-services name)
        (start-tinyproxy name)
        (sleep 6)
        (setf *dev-folder* folder *dev-port* vpn-port *dev-container* name)
        (let ((client (dev-client)) (proxy nil))
          (unwind-protect
               (progn
                 (setf proxy (dev-proxy client :port 11099))
                 (sleep 1)
                 ;; URLs for each path, all resolving to the same nginx file.
                 (flet ((erebus-small () (fetch "http://10.8.0.1/" :proxy '("127.0.0.1" 11099)))
                        (erebus-large () (fetch "http://10.8.0.1/man/bench.bin" :proxy '("127.0.0.1" 11099)))
                        (tiny-small () (fetch "http://127.0.0.1/" :proxy (list "127.0.0.1" tiny-port)))
                        (tiny-large () (fetch "http://127.0.0.1/man/bench.bin" :proxy (list "127.0.0.1" tiny-port)))
                        (direct-small () (fetch (format nil "http://127.0.0.1:~a/" nginx-port)))
                        (direct-large () (fetch (format nil "http://127.0.0.1:~a/man/bench.bin" nginx-port))))
                   ;; warmup (handshakes, nginx worker spin-up, etc.)
                   (erebus-small) (tiny-small) (direct-small)

                   (format t "~%=========================================================================~%")
                   (format t "Small responses (nginx 404, ~~146 B) -- request/round-trip overhead~%")
                   (format t "=========================================================================~%")
                   (bench "erebus" *small-iterations* #'erebus-small)
                   (bench "tinyproxy" *small-iterations* #'tiny-small)
                   (bench "direct" *small-iterations* #'direct-small)

                   (format t "~%=========================================================================~%")
                   (format t "Large responses (~a B) -- throughput~%" *large-bytes*)
                   (format t "=========================================================================~%")
                   (let ((e (bench "erebus" *large-iterations* #'erebus-large))
                         (tp (bench "tinyproxy" *large-iterations* #'tiny-large))
                         (d (bench "direct" *large-iterations* #'direct-large)))
                     (format t "~%erebus is ~,1fx slower than tinyproxy and ~,1fx slower than direct (large)~%"
                             (/ e tp) (/ e d)))))
            (when proxy (ignore-errors (hunchentoot:stop proxy)))
            (ignore-errors (disconnect client)))))
    (error (e) (format t "~&STRESS-ERROR: ~a~%" e)))
  (format t "~&== tearing down ==~%")
  (ignore-errors (cleanup-container name folder)))

(format t "~&== done ==~%")
(uiop:quit)
