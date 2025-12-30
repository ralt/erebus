(in-package #:erebus)

(defun main (vpn-host vpn-port)
  (setf *random-state* (make-random-state t)) ;; randomness is important for us.
  (let ((openvpn-client (make-instance 'openvpn-client
                                       :host vpn-host
                                       :port vpn-port)))
    (connect openvpn-client)
    (unwind-protect
         (progn)
      (disconnect openvpn-client))))
