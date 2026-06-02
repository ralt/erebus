(in-package #:erebus/test)

(def-suite* erebus/connect :in erebus)

(test connect-to-vpn-static-key-udp
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (let ((socket (usocket:socket-connect "localhost" vpn-local-port :protocol :datagram)))
      (usocket:socket-close socket)
      ;; just checking we didn't raise any conditions
      (is (= 1 1)))))
