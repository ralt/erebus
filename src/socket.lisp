(in-package #:erebus)

(defclass %socket-stream (gs:fundamental-binary-input-stream
                          gs:fundamental-binary-output-stream)
  ((%buffer :accessor %buffer)
   ;; inbound bytes received from the network but not yet consumed by a
   ;; reader, plus our read cursor into them. This lets a single TCP
   ;; segment satisfy several small reads, and lets a single large read
   ;; span several segments (i.e. TCP fragmentation).
   (%read-buffer :accessor %read-buffer)
   (%read-pos :accessor %read-pos :initform 0)
   (%socket :initarg :socket :accessor %socket)))

(defmethod initialize-instance :after ((s %socket-stream) &key)
  (setf (%buffer s) (make-array 0 :element-type 'octet))
  (setf (%read-buffer s) (make-array 0 :element-type 'octet)))

(defun %send-ack (socket)
  (send-packet (client socket) +tcp-protocol+ (%key socket)
               (%make-ipv4-tcp-packet (%src-ip socket) (%src-port socket)
                                      (%dst-ip socket) (%dst-port socket)
                                      :ack 1
                                      :seqno (%seqno socket)
                                      :ackno (%ackno socket))))

(defun %fill-read-buffer (s)
  "Receive the next inbound TCP segment, acknowledge it and stash its
payload in the stream's read buffer. Pure ACKs (no payload, no FIN) are
skipped. Returns T when payload is available, or NIL when the peer closed
the connection (FIN) without further data."
  (let* ((socket (%socket s))
         (client (client socket)))
    (loop
      (destructuring-bind (tcp-header rest-stream size)
          (receive-packet client +tcp-protocol+ (%key socket))
        (let ((fin (= 1 (tcp-header-fin tcp-header))))
          (cond
            ((> size 0)
             (let ((buffer (make-array size :element-type 'octet)))
               (read-sequence buffer rest-stream)
               (%next-ackno socket (+ size (if fin 1 0)))
               (%send-ack socket)
               (when fin (setf (%eof socket) t))
               (setf (%read-buffer s) buffer)
               (setf (%read-pos s) 0)
               (return-from %fill-read-buffer t)))
            (fin
             (%next-ackno socket 1)
             (%send-ack socket)
             (setf (%eof socket) t)
             (setf (%read-buffer s) (make-array 0 :element-type 'octet))
             (setf (%read-pos s) 0)
             (return-from %fill-read-buffer nil))
            ;; a bare ACK with no payload: nothing to deliver, wait for
            ;; the next segment.
            (t nil)))))))

(defun %read-buffer-empty-p (s)
  (>= (%read-pos s) (length (%read-buffer s))))

(defmethod gs:stream-read-sequence ((s %socket-stream) sequence start end &key)
  (let ((pos start))
    (loop
      (when (>= pos end)
        (return pos))
      (when (%read-buffer-empty-p s)
        (when (%eof (%socket s))
          (return pos))
        (unless (%fill-read-buffer s)
          (return pos)))
      (let* ((available (- (length (%read-buffer s)) (%read-pos s)))
             (wanted (- end pos))
             (n (min available wanted)))
        (replace sequence (%read-buffer s)
                 :start1 pos :end1 (+ pos n)
                 :start2 (%read-pos s) :end2 (+ (%read-pos s) n))
        (incf pos n)
        (incf (%read-pos s) n)))))

(defmethod gs:stream-read-byte ((s %socket-stream))
  (when (%read-buffer-empty-p s)
    (when (%eof (%socket s))
      (return-from gs:stream-read-byte :eof))
    (unless (%fill-read-buffer s)
      (return-from gs:stream-read-byte :eof)))
  (prog1 (aref (%read-buffer s) (%read-pos s))
    (incf (%read-pos s))))

(defmethod gs:stream-write-sequence ((s %socket-stream) sequence start end &key)
  ;; TODO: should we auto-(finish-output) at some point?
  (setf (%buffer s) (concatenate 'octet-vector (%buffer s) (subseq sequence start end))))

(defconstant +max-tcp-payload+ 1400
  "Largest TCP payload we put in a single outgoing segment. The resulting
IP packet (payload + 20 IP + 20 TCP) must stay within the VPN tun MTU
(typically 1500); anything larger is dropped on the wire rather than
fragmented, so we segment the buffer ourselves.")

(defmethod gs:stream-finish-output ((s %socket-stream))
  ;; The buffered payload may be larger than a single segment can carry,
  ;; so split it into <= +max-tcp-payload+ chunks and send each as its own
  ;; TCP segment, waiting for each to be acknowledged.
  (let* ((data (%buffer s))
         (total (length data)))
    (when (> total 0)
      (loop for start from 0 below total by +max-tcp-payload+
            for end = (min (+ start +max-tcp-payload+) total)
            do (%send-segment s (subseq data start end) (>= end total)))
      (setf (%buffer s) (make-array 0 :element-type 'octet)))))

(defun %send-segment (s payload push-p)
  "Send PAYLOAD as a single TCP segment and wait for its acknowledgement.
PUSH-P sets the PSH flag, used for the last segment of a write."
  (let* ((socket (%socket s))
         (client (client socket))
         (tcp-packet (%make-ipv4-tcp-packet (%src-ip socket) (%src-port socket)
                                            (%dst-ip socket) (%dst-port socket)
                                            :ack 1
                                            :psh (if push-p 1 0)
                                            :seqno (%seqno socket)
                                            :ackno (%ackno socket)
                                            :data payload)))
    (send-packet client +tcp-protocol+ (%key socket) tcp-packet)
    (%next-seqno socket (length payload))
    (%await-ack s)))

(defun %await-ack (s)
  "Wait until the peer acknowledges our outgoing data. Any payload that
piggybacks on the acknowledgement is stashed so it is not lost."
  (let* ((socket (%socket s))
         (client (client socket)))
    (loop
      (destructuring-bind (tcp-header rest-stream size)
          (receive-packet client +tcp-protocol+ (%key socket))
        (let ((fin (= 1 (tcp-header-fin tcp-header))))
          (when (> size 0)
            (let ((buffer (make-array size :element-type 'octet)))
              (read-sequence buffer rest-stream)
              (%next-ackno socket (+ size (if fin 1 0)))
              (%send-ack socket)
              (%stash-read s buffer)))
          (when (and fin (zerop size))
            (%next-ackno socket 1)
            (%send-ack socket)
            (setf (%eof socket) t))
          (when (= 1 (tcp-header-ack tcp-header))
            (return)))))))

(defun %stash-read (s new-bytes)
  "Prepend any still-unconsumed read bytes to NEW-BYTES and make the
result the new read buffer."
  (setf (%read-buffer s)
        (concatenate 'octet-vector
                     (subseq (%read-buffer s) (%read-pos s))
                     new-bytes))
  (setf (%read-pos s) 0))

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
   (%ackno :accessor %ackno)
   ;; set once the peer has sent us a FIN
   (%eof :accessor %eof :initform nil)
   ;; set once we have torn the connection down
   (%closed :accessor %closed :initform nil)))

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
  (declare (ignore protocol))
  (make-instance 'client-socket
                 :client client
                 :protocol :stream   ; only supported protocol for now
                 :host host
                 :port port))

(defmethod socket-close ((s client-socket))
  "Tear down the TCP connection with an orderly FIN handshake. Any
inbound data still in flight is drained and discarded. Safe to call more
than once."
  (when (%closed s)
    (return-from socket-close))
  (setf (%closed s) t)
  (let ((client (client s))
        (stream (socket-stream s)))
    (handler-case
        (progn
          ;; flush anything still buffered before closing our side.
          (finish-output stream)
          ;; send our FIN (a FIN consumes one sequence number).
          (send-packet client +tcp-protocol+ (%key s)
                       (%make-ipv4-tcp-packet (%src-ip s) (%src-port s)
                                              (%dst-ip s) (%dst-port s)
                                              :fin 1
                                              :ack 1
                                              :seqno (%seqno s)
                                              :ackno (%ackno s)))
          (%next-seqno s)
          ;; drain until the peer has sent its FIN too, discarding any
          ;; remaining payload.
          (loop until (%eof s)
                do (%fill-read-buffer stream)))
      ;; a reset (or the reader thread going away) just means the
      ;; connection is already gone; nothing left to clean up on the wire.
      (econnreset () nil)
      (error () nil))
    (remove-connection client +tcp-protocol+ (%key s))
    (release-client-port client (%src-port s))))
