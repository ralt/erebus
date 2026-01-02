(defpackage #:erebus
  (:use #:cl)
  (:local-nicknames (#:bin #:lisp-binary)
                    (#:u #:usocket)
                    (#:b64 #:base64)
                    (#:ic #:ironclad)
                    (#:fs #:flexi-streams)))

(in-package #:erebus)
