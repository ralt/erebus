(in-package #:erebus)

(defclass vpn-connection ()
  ((protocol :initarg :protocol :reader protocol)
   (host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (reader-callback :initarg :reader-callback :reader reader-callback)
   (error-callback :initarg :error-callback :reader error-callback)
   (%socket :accessor %socket)
   (%reader-thread :accessor %reader-thread)
   (%writer-thread :accessor %writer-thread)
   (%writer-queue :accessor %writer-queue)
   (%client-ports :accessor %client-ports :initform nil)))

(defmethod connect ((c vpn-connection))
  (setf (%socket c)
        (u:socket-connect (host c) (port c)
                          :protocol (protocol c)
                          :element-type 'octet))
  (setf (%writer-queue c) (lp.q:make-queue))
  (setf (%reader-thread c) (bt:make-thread (%reader-loop c) :name "reader thread"))
  (setf (%writer-thread c) (bt:make-thread (%writer-loop c) :name "writer thread")))

(defmethod disconnect ((c vpn-connection))
  (lp.q:push-queue 'stop (%writer-queue c))
  (bt:join-thread (%writer-thread c))
  (when (eq (protocol c) :stream)
    (u:socket-shutdown (%socket c) :io))
  (u:socket-close (%socket c))
  (bt:destroy-thread (%reader-thread c)))

(defun %reader-loop (c)
  (lambda ()
    (block reader
      (loop
        (handler-case
            (cond
              ((eq (protocol c) :datagram)
               (multiple-value-bind (buffer size)
                   (u:socket-receive (%socket c) nil #xffff)
                 (handler-case
                     (funcall (reader-callback c) buffer size)
                   (error (condition)
                     (format t "error in reader callback: ~a~%" condition)))))
              ((eq (protocol c) :stream)
               ;; Unlike datagrams, a TCP transport is a single framed
               ;; byte stream: an error here (EOF, or a desync we can't
               ;; resync from) means the connection is gone, so let it
               ;; propagate to the outer handler and stop the loop rather
               ;; than spinning on the same broken stream.
               (funcall (reader-callback c) (u:socket-stream (%socket c)))))
          (error (condition)
            (format t "error in reader loop: ~a~%" condition)
            (funcall (error-callback c) condition)
            (return-from reader)))))))

(defun %writer-loop (c)
  (lambda ()
    (block writer
      (loop
        (let ((item (lp.q:pop-queue (%writer-queue c))))
          (when (eq item 'stop)
            (return-from writer))
          (handler-case
              (cond ((eq (protocol c) :datagram)
                     (u:socket-send (%socket c) item (length item)))
                    ((eq (protocol c) :stream)
                     (let ((stream (u:socket-stream (%socket c))))
                       (write-sequence item stream)
                       (finish-output stream))))
            (error (c)
              (format t "error in writer loop: ~a~%" c))))))))

(defmethod send ((c vpn-connection) packet)
  (lp.q:push-queue packet (%writer-queue c)))

(defmethod find-free-client-port ((c vpn-connection))
  (loop
    (let ((port (+ 30000 (random 30000))))
      (unless (member port (%client-ports c))
        (push port (%client-ports c))
        (return-from find-free-client-port port)))))

(defmethod release-client-port ((c vpn-connection) port)
  (setf (%client-ports c) (remove port (%client-ports c))))
