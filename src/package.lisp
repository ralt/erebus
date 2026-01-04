(defpackage #:erebus
  (:use #:cl)
  (:local-nicknames (#:bin #:lisp-binary)
                    (#:u #:usocket)
                    (#:ic #:ironclad)
                    (#:fs #:flexi-streams)
                    (#:bt #:bordeaux-threads)
                    (#:lp.q #:lparallel.queue))
  (:export #:connect
           #:ping
           #:disconnect
           #:openvpn-client-static-key))
