(in-package #:erebus)

(defclass %socket-stream (gs:fundamental-binary-input-stream
                          gs:fundamental-binary-output-stream)
  ((%buffer :accessor %buffer)
   (%socket :initarg :socket :accessor %socket)))

(defmethod initialize-instance :after ((s %socket-stream) &key)
  (setf (%buffer s) (make-array 0 :element-type 'octet)))

(defmethod gs:stream-read-sequence ((s %socket-stream) sequence start end &key)
  (let* ((socket (%socket s))
         (client (client socket)))
    (destructuring-bind (tcp-header rest-stream size)
        (receive-packet client +tcp-protocol+ (%key socket))
      (declare (ignore tcp-header))
      (%next-ackno socket size)
      (send-packet client +tcp-protocol+ (%key socket)
                    (%make-ipv4-tcp-packet (%src-ip socket) (%src-port socket)
                                           (%dst-ip socket) (%dst-port socket)
                                           :ack 1
                                           :seqno (%seqno socket)
                                           :ackno (%ackno socket)))

      (read-sequence sequence rest-stream))))

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
                                            :data (%buffer s))))
    (send-packet client +tcp-protocol+ (%key socket) tcp-packet)
    (setf (%seqno socket) (+ (%seqno socket) (length (%buffer s))))

    (destructuring-bind (tcp-header rest-stream size)
        (receive-packet client +tcp-protocol+ (%key socket))
      (declare (ignore rest-stream size))   ; it's going to be empty anyway
      ;; verify ack
      (assert (= 1 (tcp-header-ack tcp-header))))

    ;; TODO: figure out how to reset the fill-pointer?
    (setf (%buffer s) (make-array 0 :element-type 'octet))))

(defclass client-socket ()
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
   (%ackno :accessor %ackno)))

(defmethod %key ((s client-socket))
  (list (%src-ip s) (%src-port s) (%dst-ip s) (%dst-port s)))

(defmethod %next-seqno ((s client-socket) &optional (delta 1))
  (mod (incf (%seqno s) delta) +max-32-bytes+))

(defmethod %next-ackno ((s client-socket) &optional (delta 1))
  (mod (incf (%ackno s) delta) +max-32-bytes+))

(defmethod initialize-instance :after ((s client-socket) &key)
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
    (send-packet client +tcp-protocol+ key tcp-packet)

    (destructuring-bind (tcp-header rest-stream size)
        (receive-packet client +tcp-protocol+ key)
      (declare (ignore rest-stream size))
      ;; verify syn-ack is valid
      (assert (= 1 (tcp-header-syn tcp-header)))
      (assert (= 1 (tcp-header-ack tcp-header)))
      (assert (= (mod (1+ (%seqno s)) +max-32-bytes+) (tcp-header-ackno tcp-header)))
      (setf (%ackno s) (tcp-header-seqno tcp-header))

      ;; ack
      (send-packet client
                    +tcp-protocol+
                    key
                    (%make-ipv4-tcp-packet src-ip src-port
                                           dst-ip dst-port
                                           :seqno (%next-seqno s)
                                           :ackno (%next-ackno s)
                                           :ack 1))

      ;; expose the stream once connection is established
      (setf (%stream s) (make-instance '%socket-stream :socket s)))))

(defun socket-connect (client &key (protocol :stream) host port)
  (make-instance 'client-socket
                 :client client
                 :protocol :stream   ; only supported protocol for now
                 :host host
                 :port port))
