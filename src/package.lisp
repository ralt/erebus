(defpackage #:erebus
  (:use #:cl)
  (:local-nicknames (#:bin #:lisp-binary)
                    (#:u #:usocket)
                    (#:ic #:ironclad)
                    (#:fs #:flexi-streams)
                    (#:bt #:bordeaux-threads)
                    (#:lp.q #:lparallel.queue)
                    (#:lt #:local-time)
                    (#:cli #:clingon))
  (:export #:connect
           #:ping
           #:disconnect
           #:openvpn-client-static-key
           #:openvpn-connect))
