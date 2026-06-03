;;;; Throwaway manual end-to-end check of the dev-* workflow.
;;;; Run from the project root with:
;;;;   sbcl --script t/manual-verify.lisp
(load "~/quicklisp/setup.lisp")
(push (truename ".") asdf:*central-registry*)
(ql:quickload :erebus/test :verbose nil)
(in-package :erebus/test)

(handler-case
    (progn
      (format t "~&== bringing up dev container ==~%")
      (dev-vpn-up :pre "python3 -c \"open('/usr/share/man/big.txt','w').write('A'*20000)\"")
      ;; give openvpn a moment to finish starting inside the container
      (sleep 5)
      (let ((client (dev-client))
            (proxy nil))
        (unwind-protect
             (progn
               (setf proxy (dev-proxy client))
               (sleep 1)
               (format t "~&== GET / (expect 404) ==~%")
               (multiple-value-bind (body status)
                   (drakma:http-request "http://10.8.0.1"
                                        :proxy '("127.0.0.1" 11023)
                                        :keep-alive t :close nil)
                 (format t "status=~a body-length=~a~%" status (length body)))
               (format t "~&== GET /man/big.txt (expect 20000 bytes, fragmented) ==~%")
               (multiple-value-bind (body status)
                   (drakma:http-request "http://10.8.0.1/man/big.txt"
                                        :proxy '("127.0.0.1" 11023)
                                        :keep-alive t :close nil)
                 (format t "status=~a body-length=~a all-A=~a~%"
                         status (length body)
                         (and (stringp body) (every (lambda (c) (char= c #\A)) body))))
               (format t "~&== GET / with Connection: close ==~%")
               (multiple-value-bind (body status)
                   (drakma:http-request "http://10.8.0.1"
                                        :proxy '("127.0.0.1" 11023)
                                        :close t)
                 (format t "status=~a body-length=~a~%" status (length body))))
          (when proxy (hunchentoot:stop proxy))
          (disconnect client))))
  (error (e) (format t "~&MANUAL-VERIFY-ERROR: ~a~%" e)))

(format t "~&== tearing down dev container ==~%")
(ignore-errors (dev-vpn-down))
(format t "~&== done ==~%")
(uiop:quit)
