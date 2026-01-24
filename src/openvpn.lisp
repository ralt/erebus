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
   (%socket :accessor %socket)
   (%ping-thread :accessor %ping-thread)))

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
  (dolist (protocol (list +icmp-protocol+ +tcp-protocol+))
    (setf (gethash protocol (%connections c)) (make-hash-table :test #'equal)))
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
  (connect (%vpn-connection c))
  (setf (%ping-thread c) (bt:make-thread (%ping-loop c) :name "ping thread")))

(defmethod disconnect ((c openvpn-client-static-key))
  (bt:destroy-thread (%ping-thread c))
  (disconnect (%vpn-connection c)))

(bin:defbinary %ping-packet (:byte-order :big-endian)
  (magic #x2a187bf3641eb4cb07ed2d0a981fc748
         :type (bin:magic :actual-type (unsigned-byte 128)
                          :value #x2a187bf3641eb4cb07ed2d0a981fc748)))

(defun %ping-loop (c)
  (lambda ()
    (loop
      (%send-packet c nil nil (make-%ping-packet))
      (sleep 10) ; TODO: should this ping interval be configurable?
      )))

(defmethod ping ((c openvpn-client-static-key) dst-address)
  (let* ((dst-ip (string-ipv4-address-to-integer dst-address))
         (key (random #xff))
         (ipv4-icmp-packet (%make-ipv4-icmp-packet (%client-ip-address c)
                                                   dst-ip
                                                   key)))
    (%send-packet c +icmp-protocol+ key ipv4-icmp-packet)))

(bin:defbinary %tcp-packet-length (:byte-order :big-endian)
  (length 0 :type (unsigned-byte 16)))

(defun %send-packet (c protocol key packet)
  (let ((serialized-packet (%serialize-packet c packet))
        (queue (lp.q:make-queue)))
    (when protocol
      (bt:with-lock-held ((%connections-lock c))
        (setf (gethash key (gethash protocol (%connections c))) queue)))
    (send (%vpn-connection c)
          (cond ((eq (protocol c) :stream)
                 (concatenate 'octet-vector (fs:with-output-to-sequence (s)
                                              (bin:write-binary
                                               (make-%tcp-packet-length
                                                :length (length serialized-packet))
                                               s))
                              serialized-packet))
                ((eq (protocol c) :datagram) serialized-packet)))
    ;; we only want to wait for ICMP, other protocols are rather stream oriented
    (when (eq protocol +icmp-protocol+)
      (let ((result (lp.q:pop-queue queue)))
        (when (eq (type-of result) 'condition)
          (error result))
        result))))

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
                          (lp.q:push-queue nil queue)))))

                   ((= protocol +tcp-protocol+)
                    ;; TODO: handle RST, FIN
                    (let* ((tcp-header (bin:read-binary 'tcp-header rest-stream))
                           (src-ip (ipv4-header-src-ip packet-header))
                           (src-port (tcp-header-src-port tcp-header))
                           (dst-ip (ipv4-header-dst-ip packet-header))
                           (dst-port (tcp-header-dst-port tcp-header))
                           (key (list dst-ip dst-port src-ip src-port)))
                      (bt:with-lock-held ((%connections-lock c))
                        (let ((queue (gethash key (gethash protocol (%connections c)))))
                          ;; TODO: not finding the key here should return a connection refused
                          (lp.q:push-queue (list tcp-header rest-stream) queue)))))))))))

(defun %receive-packet (c protocol key)
  (let ((queue))
    (bt:with-lock-held ((%connections-lock c))
      (setf queue (gethash key (gethash protocol (%connections c)))))
    ;; make sure we wait for new item *without* holding the lock, it
    ;; could wait for a while and we want other packets to be
    ;; processed in the meantime.
    (lp.q:pop-queue queue)))

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
          (bin:read-binary 'openvpn-packet-id p) ; discard replay
                                        ; protection for now
          (read-byte p)             ; compression byte, ignore for now
          (let ((first-byte (fs:peek-byte p)))
            (cond ((= first-byte #x45)  ; IP packet
                   (values :ip (bin:read-binary 'ipv4-header p) p))
                  ((= first-byte #x2A)  ; PING packet
                   (let ((buffer (make-array 16 :element-type 'octet)))
                     (read-sequence buffer p)
                     (values :ping buffer)))
                  ((= first-byte #x28)  ; OCC packet; ignore
                   :ping))))))))

(defmethod find-free-client-port ((c openvpn-client-static-key))
  (find-free-client-port (%vpn-connection c)))

(defun %integer-to-octets (n size)
  (let ((buffer (make-array size :element-type 'octet)))
    (u:integer-to-octet-buffer n buffer size)))

(defclass %socket-stream (gs:fundamental-binary-input-stream
                          gs:fundamental-binary-output-stream)
  ((%buffer :accessor %buffer)
   (%socket :initarg :socket :accessor %socket)))

(defmethod initialize-instance :after ((s %socket-stream) &key)
  (setf (%buffer s) (make-array 0 :element-type 'octet)))

(defmethod gs:stream-read-sequence ((s %socket-stream) sequence start end &key)
  (let* ((socket (%socket s))
         (client (client socket)))
    (destructuring-bind (tcp-header rest-stream)
        (%receive-packet client +tcp-protocol+ (%key socket))
      (setf (%ackno socket) (tcp-header-seqno tcp-header))

      (write-sequence sequence rest-stream))))

(defmethod gs:stream-write-sequence ((s %socket-stream) sequence start end &key)
  ;; TODO: should we auto-(finish-output) at some point?
  (setf (%buffer s) (concatenate 'octet-vector (%buffer s) (subseq sequence start end))))

(defmethod gs:stream-finish-output ((s %socket-stream))
  ;; wrap %buffer in an ipv4-tcp-packet and send over
  (let* ((socket (%socket s))
         (client (client socket))
         (tcp-packet (%make-ipv4-tcp-packet (%src-ip socket) (%src-port socket)
                                            (%dst-ip socket) (%dst-port socket)
                                            :ack 1
                                            :seqno (%seqno socket)
                                            :ackno (%ackno socket)
                                            :window (%window socket)
                                            :data (%buffer s))))
    (%send-packet client +tcp-protocol+ (%key socket) tcp-packet)
    (setf (%seqno socket) (+ (%seqno socket) (length (%buffer s))))

    (destructuring-bind (tcp-header rest-stream)
        (%receive-packet client +tcp-protocol+ (%key socket))
      (declare (ignore rest-stream))   ; it's going to be empty anyway
      ;; verify ack
      (assert (= 1 (tcp-header-ack tcp-header))))

    ;; TODO: figure out how to reset the fill-pointer?
    (setf (%buffer s) (make-array 0 :element-type 'octet))))

(defclass openvpn-client-socket ()
  ((client :initarg :client :reader client)
   (protocol :initarg :protocol :reader protocol)
   (host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (stream :reader socket-stream :accessor %stream)
   (%src-ip :accessor %src-ip)
   (%src-port :accessor %src-port)
   (%dst-ip :accessor %dst-ip)
   (%dst-port :accessor %dst-port)
   (%seqno :accessor %seqno :initform 0)
   (%ackno :accessor %ackno)
   (%window :accessor %window)))

(defmethod %key ((s openvpn-client-socket))
  (list (%src-ip s) (%src-port s) (%dst-ip s) (%dst-port s)))

(defmethod %next-seqno ((s openvpn-client-socket))
  (mod (incf (%seqno s)) +max-32-bytes+))

(defmethod %next-ackno ((s openvpn-client-socket))
  (mod (incf (%ackno s)) +max-32-bytes+))

(defun openvpn-connect (client &key (protocol :stream) host port)
  (make-instance 'openvpn-client-socket
                 :client client
                 :protocol :stream   ; only supported protocol for now
                 :host host
                 :port port))

(defmethod initialize-instance :after ((s openvpn-client-socket) &key)
  ;; establish tcp connection
  (setf (%seqno s) (random #xffffffff))
  (let* ((client (client s))
         (src-port (find-free-client-port client))
         (src-ip (%client-ip-address client))
         (dst-ip (string-ipv4-address-to-integer (host s)))
         (dst-port (port s))
         (key (list src-ip src-port dst-ip dst-port))
         (tcp-packet (%make-ipv4-tcp-packet src-ip src-port
                                            dst-ip dst-port
                                            :seqno (%next-seqno s)
                                            :syn 1)))
    (setf (%src-ip s) src-ip)
    (setf (%src-port s) src-port)
    (setf (%dst-ip s) dst-ip)
    (setf (%dst-port s) dst-port)

    ;; syn
    (%send-packet client +tcp-protocol+ key tcp-packet)

    (destructuring-bind (tcp-header rest-stream)
        (%receive-packet client +tcp-protocol+ key)
      (declare (ignore rest-stream))
      ;; verify syn-ack is valid
      (assert (= 1 (tcp-header-syn tcp-header)))
      (assert (= 1 (tcp-header-ack tcp-header)))
      (assert (= (mod (1+ (%seqno s)) +max-32-bytes+) (tcp-header-ackno tcp-header)))
      (setf (%ackno s) (tcp-header-seqno tcp-header))
      (setf (%window s) (tcp-header-window tcp-header))

      ;; ack
      (%send-packet client
                    +tcp-protocol+
                    key
                    (%make-ipv4-tcp-packet src-ip src-port
                                           dst-ip dst-port
                                           :seqno (%next-seqno s)
                                           :ackno (%next-ackno s)
                                           :window (%window s)
                                           :ack 1))

      ;; expose the stream once connection is established
      (setf (%stream s) (make-instance '%socket-stream :socket s)))))
