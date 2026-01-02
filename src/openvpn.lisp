(in-package #:erebus)

(defclass openvpn-client-static-key ()
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (client-ip :initarg :client-ip :reader client-ip)
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

(defmethod initialize-instance :after ((c openvpn-client) &key)
  (setf (%client-ip-address c) (%string-ipv4-address-to-integer (client-ip c)))
  (let ((static-key-binary-value (%parse-static-key (%static-key c))))
    (setf (%cipher c) (ic:make-cipher :aes
                                      :mode :cbc
                                      :key (subseq static-key-binary-value 0 32)
                                      :initialization-vector (ic:random-bits 128)))
    (setf (%hmac c) (ic:make-hmac (subseq static-key-binary-value 64 96) :sha256))))

(defmethod connect ((c openvpn-client))
  (setf (%socket c)
        (u:socket-connect (host c) (port c)
                          :protocol :datagram
                          :element-type '(unsigned-byte 8))))

(defmethod disconnect ((c openvpn-client))
  (socket-close (%socket c)))

(defmethod ping ((c openvpn-client) dst-address)
  (let* ((dst-ip (string-ipv4-address-to-integer dst-address))
         (ipv4-icmp-packet (%make-ipv4-icmp-packet (%client-ip-address c)
                                                   dst-ip)))
    ;; use ironclad to encrypt this packet and then send UDP packets?
    ))
