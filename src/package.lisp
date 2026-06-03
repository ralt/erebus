(defpackage #:erebus
  (:use #:cl)
  (:local-nicknames (#:bin #:lisp-binary)
                    (#:u #:usocket)
                    (#:ic #:ironclad)
                    (#:fs #:flexi-streams)
                    (#:bt #:bordeaux-threads)
                    (#:lp.q #:lparallel.queue)
                    (#:lt #:local-time)
                    (#:cli #:clingon)
                    (#:a #:alexandria)
                    (#:gs #:trivial-gray-streams)
                    (#:h #:hunchentoot)
                    (#:b #:babel))
  (:export #:connect
           #:ping
           #:disconnect
           #:openvpn-client-static-key
           #:ipsec-client
           #:openvpn-connect
           #:socket-connect
           #:socket-stream
           #:socket-close
           #:socket-listen
           #:socket-accept
           #:socket-unlisten
           #:server-socket
           #:expose
           #:unexpose
           #:econnreset
           #:acceptor))
