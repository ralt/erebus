(in-package #:erebus)

(defclass openvpn-client-static-key ()
  ((protocol :initarg :protocol :reader protocol :initform :datagram)
   (host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (client-ip :initarg :client-ip :reader client-ip)
   (cipher :initarg :cipher :reader cipher)
   (auth :initarg :auth :reader auth)
   (secret :initarg :secret :reader secret)
   (key-direction :initarg :key-direction :reader key-direction :initform nil)
   (%vpn-connection :accessor %vpn-connection)
   (%packet-id-counter :accessor %packet-id-counter :initform 0)
   (%connections :accessor %connections)
   (%connections-lock :accessor %connections-lock)
   (%client-ip-address :accessor %client-ip-address)
   (%cipher-type :accessor %cipher-type)
   (%cipher-mode :accessor %cipher-mode)
   (%cipher-encrypt-key :accessor %cipher-encrypt-key)
   (%cipher-decrypt-key :accessor %cipher-decrypt-key)
   (%cipher-block-length :accessor %cipher-block-length)
   (%hmac-type :accessor %hmac-type)
   (%hmac-encrypt-key :accessor %hmac-encrypt-key)
   (%hmac-decrypt-key :accessor %hmac-decrypt-key)
   (%hmac-length :accessor %hmac-length)
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

(defvar *ciphers* '(("AES" . :aes)
                    ("ARIA" . :aria)
                    ("CAMELLIA" . :camellia)))
(defvar *cipher-modes* '(("CBC" . :cbc)))
(defvar *digests* '(("MD5" . :md5)
                    ("SHA1" . :sha1)
                    ("RIPEMD160" . :ripemd-160)
                    ("MD4" . :md4)
                    ("SHA256" . :sha256)
                    ("SHA384" . :sha384)
                    ("SHA512" . :sha512)
                    ("SHA224" . :sha224)
                    ("whirlpool" . :whirlpool)
                    ("BLAKE2s256" . :blake2s/256)
                    ("SHA3-224" . :sha3/224)
                    ("SHA3-256" . :sha3/256)
                    ("SHA3-384" . :sha3/384)
                    ("SHA3-512" . :sha3/512)
                    ("SHAKE128" . :shake128)
                    ("SHAKE256" . :shake256)
                    ("SM3" . :sm3)))

(defun %parse-cipher (cipher)
  (let ((parts (uiop:split-string cipher :separator '(#\-))))
    (values (cdr (assoc (first parts) *ciphers* :test #'string=))
            (parse-integer (second parts))
            (cdr (assoc (third parts) *cipher-modes* :test #'string=)))))

(defmethod initialize-instance :after ((c openvpn-client-static-key) &key)
  (setf (%client-ip-address c) (string-ipv4-address-to-integer (client-ip c)))
  (setf (%connections c) (make-hash-table))
  ;; Initialize an empty hash table of connections for each protocol
  ;; we support so that we don't have to try doing that every time we
  ;; make a new connection
  (dolist (protocol (list +icmp-protocol+))
    (setf (gethash protocol (%connections c)) (make-hash-table)))
  (setf (%connections-lock c) (bt:make-lock))
  (setf (%vpn-connection c)
        (make-instance 'vpn-connection
                       :protocol (protocol c)
                       :host (host c)
                       :port (port c)
                       :reader-callback (cond ((eq (protocol c) :datagram)
                                               (%reader-callback-udp c))
                                              ((eq (protocol c) :stream)
                                               (%reader-callback-tcp c)))
                       :error-callback (%error-callback c)))
  (let* ((hmac-type (cdr (assoc (auth c) *digests* :test #'string=)))
         (parts (uiop:split-string (secret c)))
         (secret-path (first parts))
         (key-direction (if (= (length parts) 2)
                            (let ((direction (parse-integer (second parts))))
                              (cond ((= direction 0) :normal)
                                    ((= direction 1) :inverse)))
                            (if (key-direction c)
                                (let ((direction (parse-integer (key-direction c))))
                                  (cond ((= direction 0) :normal)
                                        ((= direction 1) :inverse)))
                                :bidirectional))))
    (setf (%hmac-type c) hmac-type)
    (multiple-value-bind (type key-size mode)
        (%parse-cipher (cipher c))
      (setf (%cipher-type c) type)
      (setf (%cipher-mode c) mode)
      (setf (%cipher-block-length c) (ic:block-length type))
      (setf (%hmac-length c) (ic:digest-length hmac-type))

      (let ((static-key-binary-value (%parse-static-key secret-path))
            (cipher-encrypt-start (cond ((eq key-direction :bidirectional) 0)
                                        ((eq key-direction :normal) 0)
                                        ((eq key-direction :inverse) 128)))
            (cipher-decrypt-start (cond ((eq key-direction :bidirectional) 0)
                                        ((eq key-direction :normal) 128)
                                        ((eq key-direction :inverse) 0)))
            (hmac-encrypt-start (cond ((eq key-direction :bidirectional) 64)
                                        ((eq key-direction :normal) 64)
                                        ((eq key-direction :inverse) 192)))
            (hmac-decrypt-start (cond ((eq key-direction :bidirectional) 64)
                                      ((eq key-direction :normal) 192)
                                      ((eq key-direction :inverse) 64))))
        (setf (%cipher-encrypt-key c)
              (subseq static-key-binary-value
                      cipher-encrypt-start
                      (+ cipher-encrypt-start (/ key-size 8))))
        (setf (%cipher-decrypt-key c)
              (subseq static-key-binary-value
                      cipher-decrypt-start
                      (+ cipher-decrypt-start (/ key-size 8))))
        (setf (%hmac-encrypt-key c)
              (subseq static-key-binary-value
                      hmac-encrypt-start
                      (+ hmac-encrypt-start (%hmac-length c))))
        (setf (%hmac-decrypt-key c)
              (subseq static-key-binary-value
                      hmac-decrypt-start
                      (+ hmac-decrypt-start (%hmac-length c))))))))

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
    (%send-packet c +icmp-protocol+ key (%serialize-packet c ipv4-icmp-packet))))

(bin:defbinary %tcp-packet-length (:byte-order :big-endian)
  (length 0 :type (unsigned-byte 16)))

(defun %send-packet (c protocol key packet)
  (let ((queue (lp.q:make-queue)))
    (bt:with-lock-held ((%connections-lock c))
      (setf (gethash key (gethash protocol (%connections c))) queue))
    (send (%vpn-connection c)
          (cond ((eq (protocol c) :stream)
                 (concatenate 'octet-vector (fs:with-output-to-sequence (s)
                                              (bin:write-binary
                                               (make-%tcp-packet-length :length (length packet))
                                               s))
                              packet))
                ((eq (protocol c) :datagram) packet)))
    (let ((condition (lp.q:pop-queue queue)))
      (when condition
        (error condition)))))

(defun %reader-callback-udp (c)
  (lambda (buffer size)
    (%reader-callback c buffer size)))

(defun %reader-callback-tcp (c)
  (lambda (stream)
    (let* ((size (%tcp-packet-length-length (bin:read-binary '%tcp-packet-length stream)))
           (buffer (make-array size :element-type 'octet)))
      (%read-until stream buffer size)
      (%reader-callback c buffer size))))

(defun %read-until (stream buffer size)
  (let ((offset 0))
    (loop
      (let ((count (read-sequence buffer stream :start offset)))
        (when (= count size)
          (return-from %read-until))
        (setf size (- size count))
        (setf offset (+ offset (1- count)))))))

(defun %reader-callback (c buffer size)
  (multiple-value-bind (type packet-header rest-stream)
      (%deserialize-packet c buffer size)
    (cond ((eq type :ip)
           (let ((protocol (ipv4-header-protocol packet-header)))
             (cond ((= protocol +icmp-protocol+)
                    (let* ((icmp-packet (bin:read-binary 'icmp-packet rest-stream))
                           (key (icmp-packet-identifier icmp-packet)))
                      (bt:with-lock-held ((%connections-lock c))
                        (let ((queue (gethash key (gethash protocol (%connections c)))))
                          (remhash key (gethash protocol (%connections c)))
                          (lp.q:push-queue nil queue))))))))
          ((eq type :ping) ; for yet non-understood reasons, ICMP
                           ; packets on top of TCP are sent back as
                           ; PING packets... which don't have an
                           ; identifier. So we just guess...
           (bt:with-lock-held ((%connections-lock c))
             (maphash (lambda (key queue)
                        (declare (ignore key))
                        (lp.q:push-queue nil queue))
                      (gethash +icmp-protocol+ (%connections c))))))))

(defun %error-callback (c)
  (lambda (condition)
    ;; just push the error to all the ongoing connections
    (maphash (lambda (protocol table)
               (declare (ignore protocol))
               (maphash (lambda (key queue)
                          (declare (ignore key))
                          (lp.q:push-queue condition queue))
                        table))
             (%connections c))))

(bin:defbinary openvpn-packet-id (:byte-order :big-endian)
  (packet-id 0 :type (unsigned-byte 32))
  (timestamp 0 :type (unsigned-byte 32)))

(defconstant +NO_COMPRESS_BYTE+ #xFA)

(defun %serialize-packet (c packet)
  (let* ((iv (%integer-to-octets (ic:random-bits (* 8 (%cipher-block-length c)))
                                 (%cipher-block-length c)))
         (ciphertext (ic:encrypt-message
                      (ic:make-cipher (%cipher-type c)
                                      :mode (%cipher-mode c)
                                      :key (%cipher-encrypt-key c)
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
         (hmac (ic:make-hmac (%hmac-encrypt-key c) (%hmac-type c))))
    (ic:update-hmac hmac (concatenate 'octet-vector iv ciphertext))
    (concatenate 'octet-vector (ic:hmac-digest hmac) iv ciphertext)))

(defun %deserialize-packet (c buffer size)
  (fs:with-input-from-sequence (s buffer)
    (let ((hmac (make-array (%hmac-length c) :element-type 'octet))
          (iv (make-array (%cipher-block-length c) :element-type 'octet))
          (ciphertext (make-array (- size (%hmac-length c) (%cipher-block-length c))
                                  :element-type 'octet)))
      (read-sequence hmac s)
      (read-sequence iv s)
      (read-sequence ciphertext s)

      (let ((body (concatenate 'octet-vector iv ciphertext))
            (supposed-hmac (ic:make-hmac (%hmac-decrypt-key c) (%hmac-type c))))
        (ic:update-hmac supposed-hmac body)
        (assert (ic:constant-time-equal hmac (ic:hmac-digest supposed-hmac))))

      (let ((decrypted-packet (ic:decrypt-message
                               (ic:make-cipher (%cipher-type c)
                                               :mode (%cipher-mode c)
                                               :key (%cipher-decrypt-key c)
                                               :padding :pkcs7
                                               :initialization-vector iv)
                               ciphertext)))
        (fs:with-input-from-sequence (p decrypted-packet)
          (bin:read-binary 'openvpn-packet-id p) ; discard replay protection for now
          (read-byte p)             ; compression byte, ignore for now
          (let ((first-byte (fs:peek-byte p)))
            (cond ((= first-byte #x45)  ; IP packet
                   (values :ip (bin:read-binary 'ipv4-header p) p))
                  ((= first-byte #x2A)  ; PING packet
                   (let ((buffer (make-array 16 :element-type 'octet)))
                     (read-sequence buffer p)
                     (values :ping buffer))))))))))

(defun %integer-to-octets (n size)
  (let ((buffer (make-array size :element-type 'octet)))
    (u:integer-to-octet-buffer n buffer size)))
