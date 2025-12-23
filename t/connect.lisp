(in-package #:erebus/test)

(def-suite* erebus/connect :in erebus)

(test connect-to-vpn
  (with-docker-container (name folder vpn-local-port)
    (let ((socket (usocket:socket-connect "localhost" vpn-local-port
                                          :protocol :datagram)))
      (usocket:socket-close socket)
      ;; just checking we didn't raise any conditions
      (is (= 1 1)))))
