(in-package #:erebus)

(defclass openvpn-client-static-key ()
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (client-ip :initarg :client-ip :reader client-ip)
   (%vpn-connection :accessor %vpn-connection)
   (%connections :accessor %connections)
   (%connections-lock :accessor %connections-lock)
   (%client-ip-address :accessor %client-ip-address)
   (%static-key :initarg :static-key :reader %static-key :initform nil)
   (%cipher :accessor %cipher)
   (%hmac :accessor %hmac)
   (%socket :accessor %socket)))

(defun %parse-static-key (path)
  (apply
   #'concatenate 'vector
   (with-open-file (s path)
     (loop with collecting-p = nil
           for line = (read-line s nil nil)
           while line
           when (string= line "-----END OpenVPN Static key V1-----")
             do (setf collecting-p nil)
           when collecting-p
             collect (b64:base64-string-to-usb8-array line)
           when (string= line "-----BEGIN OpenVPN Static key V1-----")
             do (setf collecting-p t)))))

(defmethod initialize-instance :after ((c openvpn-client-static-key) &key)
  (setf (%client-ip-address c) (string-ipv4-address-to-integer (client-ip c)))
  (setf (%connections c) (make-hash-table))
  (setf (%connections-lock c) (bt:make-lock))
  (setf (%vpn-connection c) (make-instance 'vpn-connection
                                           :host (host c)
                                           :port (port c)
                                           :reader-callback (%reader-callback c)))
  (let ((static-key-binary-value (%parse-static-key (%static-key c))))
    (setf (%cipher c) (ic:make-cipher :aes
                                      :mode :cbc
                                      :key (subseq static-key-binary-value 0 32)
                                      :initialization-vector (%integer-to-octets
                                                              (ic:random-bits 128)
                                                              16)))
    (setf (%hmac c) (ic:make-hmac (subseq static-key-binary-value 64 96) :sha256))))

(defmethod connect ((c openvpn-client-static-key))
  (connect (%vpn-connection c)))

(defmethod disconnect ((c openvpn-client-static-key))
  (disconnect (%vpn-connection c)))

(defmethod ping ((c openvpn-client-static-key) dst-address)
  (let* ((dst-ip (string-ipv4-address-to-integer dst-address))
         (key (random #xff))
         (ipv4-icmp-packet (%make-ipv4-icmp-packet (%client-ip-address c)
                                                   dst-ip
                                                   key)))
    ;; use ironclad to encrypt this packet and then send UDP packets?
    (let ((queue (lp.q:make-queue)))
      (bt:with-lock-held ((%connections-lock c))
        (setf (gethash key (%connections c)) queue))
      (send (%vpn-connection c) (%encrypt-packet c ipv4-icmp-packet))
      (lp.q:pop-queue queue))))

(defun %reader-callback (c)
  (lambda (buffer size)
    (declare (ignore size))
    ;; Decrypt and then do the mapping with internal queues
    (let* ((decrypted-ipv4-icmp-packet (%decrypt-packet c buffer size))
           (key (icmp-packet-identifier decrypted-ipv4-icmp-packet)))
      (bt:with-lock-held ((%connections-lock c))
        (let ((queue (gethash key (%connections c))))
          (lp.q:push-queue nil queue))))))

(defun %encrypt-packet (c buffer)
  (ic:encrypt-message (%cipher c) buffer))

(defun %integer-to-octets (n size)
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (u:integer-to-octet-buffer n buffer size)))
