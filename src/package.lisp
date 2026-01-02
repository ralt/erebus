(defpackage #:erebus
  (:use #:cl)
  (:local-nicknames (#:bin #:lisp-binary)
                    (#:u #:usocket)
                    (#:b64 #:base64)
                    (#:ic #:ironclad)
                    (#:fs #:flexi-streams)
                    (#:bt #:bordeaux-threads)
                    (#:lp.q #:lparallel.queue))
  (:export #:connect
           #:ping
           #:disconnect
           #:openvpn-client-static-key))
