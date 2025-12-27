(defpackage #:erebus/test
  (:use :cl :fiveam :erebus)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:erebus/test)

(def-suite erebus
  :description "Erebus test suite")

(defun random-string (length)
  (funcall
   (gen-string :length (gen-integer :min length :max length)
               :elements (gen-character :code (gen-integer :min 97 :max 122)))))

(defmacro with-docker-container ((container-name container-folder vpn-local-port) &body body)
  (a:with-gensyms (erebus-test-folder
                            junk
                            dockerfile
                            ignore-me-file
                            ignore-me-filename
                            run-in-container)
    `(let* ((,erebus-test-folder
              (merge-pathnames "t/"
                               (asdf:system-source-directory :erebus/test)))
            (,junk (merge-pathnames "junk/" ,erebus-test-folder))
            (,dockerfile (probe-file
                          (merge-pathnames "Dockerfile" ,erebus-test-folder)))
            (,ignore-me-filename (merge-pathnames ".git-ignore-me-container" ,erebus-test-folder))
            (,ignore-me-file (probe-file ,ignore-me-filename)))
       (when (or
              (not ,ignore-me-file)
              (> (file-write-date ,dockerfile)
                 (file-write-date ,ignore-me-file)))
         (uiop:run-program "cd t; docker build -t ralt/erebus:latest ." :output t :error-output t)
         ;; a quick version of "touch" that updates the mtime on a new file every time it runs
         (close
          (open ,ignore-me-filename :direction :output :if-exists :supersede :if-does-not-exist :create)))
       (flet ((,run-in-container (container command)
                (uiop:run-program (format nil "docker exec -i ~a bash -c ~s" container command)
                                  :output t
                                  :error-output t)))
         (let* ((,container-name (format nil "erebus_~a" (random-string 20)))
                (,container-folder (merge-pathnames (make-pathname :directory
                                                                   (list :relative ,container-name))
                                                    ,junk))
                (,vpn-local-port (funcall (gen-integer :min 10000 :max 60000))))
           (ensure-directories-exist ,container-folder)

           (unwind-protect
                (progn
                  (uiop:run-program
                   (format nil "docker create \\
                                  --privileged \\
                                  --publish ~a:1194/udp \\
                                  --name ~a \\
                                  --volume ~a:/etc/openvpn/ \\
                                  --volume /lib/modules:/lib/modules \\
                                  ralt/erebus:latest"
                           ,vpn-local-port
                           ,container-name
                           ,container-folder)
                   :output t
                   :error-output t)
                  (uiop:run-program
                   (format nil "docker start ~a" ,container-name)
                   :output t
                   :error-output t)
                  (,run-in-container ,container-name "ovpn_genconfig -u udp://erebus.local")
                  (,run-in-container ,container-name "ovpn_initpki nopass")
                  (,run-in-container ,container-name "easyrsa build-client-full erebus nopass")
                  (,run-in-container ,container-name "ovpn_getclient erebus > /etc/openvpn/erebus.ovpn")
                  (,run-in-container ,container-name "mkdir -p /run/nginx && nginx && nohup ovpn_run &")

                  (progn ,@body))
             (uiop:run-program (format nil "docker rm --force ~a" ,container-name) :output t :error-output t)
             ;; because the folders are created as root inside the
             ;; container, the lisp process on the host will usually
             ;; not have permissions to delete it. we thus have to do
             ;; the deletion inside the container of most of the
             ;; stuff, and then we can cleanup host-side.
             (uiop:run-program
              (format nil "docker run --rm -v ~a:/etc/openvpn -i ralt/erebus:latest bash -c 'rm -rf /etc/openvpn/*'"
                      ,container-folder)
              :output t :error-output t)
             (uiop:delete-directory-tree ,container-folder :validate t)))))))
