(in-package #:erebus)

(defclass openvpn-client-static-key ()
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (client-ip :initarg :client-ip :reader client-ip)
   (%vpn-connection :accessor %vpn-connection)
   (%packet-id-counter :accessor %packet-id-counter :initform 0)
   (%connections :accessor %connections)
   (%connections-lock :accessor %connections-lock)
   (%client-ip-address :accessor %client-ip-address)
   (%static-key :initarg :static-key :reader %static-key :initform nil)
   (%cipher-key :accessor %cipher-key)
   (%hmac-key :accessor %hmac-key)
   (%socket :accessor %socket)))

(defun %hex-string-to-byte-vector (str nb)
  (let ((bytes (make-array nb :element-type 'octet)))
    (dotimes (i nb bytes)
      (setf (elt bytes i) (parse-integer (subseq str (* i 2) (* (1+ i) 2)) :radix 16)))))

(defun %parse-static-key (path)
  (apply
   #'concatenate 'octet-vector
   (with-open-file (s path)
     (loop with collecting-p = nil
           for line = (read-line s nil nil)
           while line
           when (string= line "-----END OpenVPN Static key V1-----")
             do (setf collecting-p nil)
           when collecting-p
             collect (%hex-string-to-byte-vector line 16)
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
    (setf (%cipher-key c) (subseq static-key-binary-value 0 32))
    (setf (%hmac-key c) (subseq static-key-binary-value 64 (+ 64 32)))))

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
    (let ((queue (lp.q:make-queue)))
      (bt:with-lock-held ((%connections-lock c))
        (setf (gethash key (%connections c)) queue))
      (send (%vpn-connection c) (%serialize-packet c ipv4-icmp-packet))
      (lp.q:pop-queue queue))))

(defun %reader-callback (c)
  (lambda (buffer size)
    ;; Decrypt and then do the mapping with internal queues
    (let* ((decrypted-ipv4-icmp-packet (%deserialize-packet c buffer size))
           (key (icmp-packet-identifier (ipv4-icmp-packet-icmp-packet decrypted-ipv4-icmp-packet))))
      (bt:with-lock-held ((%connections-lock c))
        (let ((queue (gethash key (%connections c))))
          (lp.q:push-queue nil queue))))))

(bin:defbinary openvpn-packet-id (:byte-order :big-endian)
  (packet-id 0 :type (unsigned-byte 32))
  (timestamp 0 :type (unsigned-byte 32)))

(defconstant +iv-length-aes-cbc+ 16)
(defconstant +NO_COMPRESS_BYTE+ #xFA)

(defun %serialize-packet (c packet)
  (let* ((iv (%integer-to-octets (random #xffffffffffffffff) +iv-length-aes-cbc+))
         (ciphertext (ic:encrypt-message
                      (ic:make-cipher :aes
                                      :mode :cbc
                                      :key (%cipher-key c)
                                      :padding :pkcs7
                                      :initialization-vector iv)
                      (coerce (fs:with-output-to-sequence (s)
                                (bin:write-binary (make-openvpn-packet-id
                                                   :packet-id (incf (%packet-id-counter c))
                                                   :timestamp (lt:timestamp-to-unix (lt:now)))
                                                  s)
                                (write-byte +NO_COMPRESS_BYTE+ s)
                                (bin:write-binary packet s))
                              'octet-vector)))
         (hmac (ic:make-hmac (%hmac-key c) :sha256)))
    (ic:update-hmac hmac (concatenate 'octet-vector iv ciphertext))
    (concatenate 'octet-vector (ic:hmac-digest hmac) iv ciphertext)))

(defun %deserialize-packet (c buffer size)
  (fs:with-input-from-sequence (s buffer)
    (let ((hmac (make-array 32 :element-type 'octet))
          (iv (make-array 16 :element-type 'octet))
          (ciphertext (make-array (- size +iv-length-aes-cbc+ 32) ; 16 = IV, 32 = HMAC
                                  :element-type 'octet)))
      (read-sequence hmac s)
      (read-sequence iv s)
      (read-sequence ciphertext s)

      (let ((body (concatenate 'octet-vector iv ciphertext))
            (supposed-hmac (ic:make-hmac (%hmac-key c) :sha256)))
        (ic:update-hmac supposed-hmac body)
        (assert (ic:constant-time-equal hmac (ic:hmac-digest supposed-hmac))))

      (let ((decrypted-packet (ic:decrypt-message
                               (ic:make-cipher :aes
                                               :mode :cbc
                                               :key (%cipher-key c)
                                               :padding :pkcs7
                                               :initialization-vector iv)
                               ciphertext)))
        (fs:with-input-from-sequence (p decrypted-packet)
          (bin:read-binary 'openvpn-packet-id p) ; discard replay protection for now
          (read-byte p) ; compression byte, ignore for now
          (bin:read-binary 'ipv4-icmp-packet p))))))

(defun %integer-to-octets (n size)
  (let ((buffer (make-array size :element-type 'octet)))
    (u:integer-to-octet-buffer n buffer size)))
