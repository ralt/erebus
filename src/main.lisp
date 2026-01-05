(in-package #:erebus)

(defun main (vpn-host vpn-port client-ip static-key)
  (setf *random-state* (make-random-state t)) ;; randomness is important for us.
  (let ((openvpn-client (make-instance 'openvpn-client-static-key
                                       :host vpn-host
                                       :port vpn-port
                                       :client-ip client-ip
                                       :static-key static-key)))
    (connect openvpn-client)
    (unwind-protect
         (ping openvpn-client "10.0.0.1")
      (disconnect openvpn-client))))
