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
  (let ((bytes (make-array nb :element-type '(unsigned-byte 8))))
    (dotimes (i nb bytes)
      (setf (elt bytes i) (parse-integer (subseq str (* i 2) (* (1+ i) 2)) :radix 16)))))

(defun %parse-static-key (path)
  (apply
   #'concatenate '(simple-array (unsigned-byte 8) (*))
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

(defconstant +P_DATA_V1+ 4)

(bin:defbinary openvpn-packet-header (:byte-order :big-endian)
  (opcode +P_DATA_V1+ :type (unsigned-byte 4))
  (key-id 0 :type (unsigned-byte 4))
  (packet-id 0 :type (unsigned-byte 32)))

(defun %serialize-packet (c packet)
  (let* ((iv (%integer-to-octets (ic:random-bits 128) 16))
         (body (concatenate '(simple-array (unsigned-byte 8) (*))
                            (fs:with-output-to-sequence (s)
                              (bin:write-binary
                               (make-openvpn-packet-header :packet-id (incf (%packet-id-counter c)))
                               s))
                            iv
                            (ic:encrypt-message
                             (ic:make-cipher :aes
                                             :mode :cbc
                                             :key (%cipher-key c)
                                             :padding :pkcs7
                                             :initialization-vector iv)
                             (coerce (fs:with-output-to-sequence (s)
                                       (bin:write-binary packet s))
                                     '(simple-array (unsigned-byte 8) (*))))))
         (hmac (ic:make-hmac (%hmac-key c) :sha256)))
    (ic:update-hmac hmac body)
    (concatenate '(simple-array (unsigned-byte 8) (*)) body (ic:hmac-digest hmac))))

(defun %deserialize-packet (c buffer size)
  (fs:with-input-from-sequence (s buffer)
    (let ((header (bin:read-binary 'openvpn-packet-header s)))
      (assert (eq (openvpn-packet-header-opcode header) +P_DATA_V1+))
      ;; TODO: replay protection using packet-id?
      (let ((iv (make-array 16 :element-type '(unsigned-byte 8)))
            (ciphertext (make-array (- size 5 16 32) :element-type '(unsigned-byte 8)))
                                        ; 5 = header, 16 = IV, 32 = HMAC
            (hmac (make-array 32 :element-type '(unsigned-byte 8))))
        (read-sequence hmac s)
        (read-sequence iv s)
        (read-sequence ciphertext s)

        (let ((body (concatenate '(simple-array (unsigned-byte 8) (*))
                                 (subseq buffer 0 (- size 32))))
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
            (bin:read-binary 'ipv4-icmp-packet p)))))))

(defun %integer-to-octets (n size)
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (u:integer-to-octet-buffer n buffer size)))
