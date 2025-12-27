(in-package #:erebus)

(defclass openvpn-client ()
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (%socket :accessor %socket)))

(defconstant +P_CONTROL_HARD_RESET_CLIENT_V2+ 7)
(defconstant +P_CONTROL_HARD_RESET_SERVER_V2+ 8)

(defbinary %openvpn-control-header ()
  (%opcode 0 :type (unsigned-byte 8))
  (%session-id 0 :type (unsigned-byte 64) :byte-order :big-endian)
  (%packet-id 0 :type (unsigned-byte 32) :byte-order :big-endian))

(defbinary %openvpn-tlv ()
  (length 0 :type (unsigned-byte 16) :byte-order :big-endian)
  (value #() :type (simple-array (unsigned-byte 8) (length))))

(defun %new-session-id ()
  ;; Generate a random 64 bits integer that will be used to identify
  ;; the ongoing session/connection.
  (random (expt 2 64)))

(defun %make-hard-reset-client-packet ()
  (with-output-to-sequence (out)
    (write-binary (make-%openvpn-control-header :%opcode +P_CONTROL_HARD_RESET_CLIENT_V2+
                                                :%session-id (%new-session-id)
                                                :%packet-id 0)
                  out)))

(defun %read-packet-values (tlvs stream)
  (let ((tlv (read-binary '%openvpn-tlv stream)))
    (if (not tlv)
        tlvs
        (%read-packet-values (append tlvs (list tlv)) stream))))

(defmethod connect ((c openvpn-client))
  (setf (%socket c)
        (socket-connect (host c) (port c)
                        :protocol :datagram
                        :element-type '(unsigned-byte 8)))

  (let ((sock (%socket c))
        (hard-reset-packet (%make-hard-reset-client-packet)))
    (socket-send sock hard-reset-packet (length hard-reset-packet))

    (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
      (multiple-value-bind (hard-reset-server-packet length)
          (socket-receive sock buffer 2048)
        (with-input-from-sequence (stream (subseq hard-reset-server-packet (1- length)))
          (let ((hard-reset-server-packet (read-binary '%openvpn-control-header stream))
                (values (%read-packet-values (list) stream)))
            (assert (= (getf hard-reset-server-packet :opcode) +P_CONTROL_HARD_RESET_SERVER_V2+))
            (format t "header: ~a, values: ~a~%" hard-reset-server-packet values)))))))

(defmethod disconnect ((c openvpn-client))
  (socket-close (%socket c)))
