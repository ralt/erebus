(in-package #:erebus)

(defclass vpn-connection ()
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (reader-callback :initarg :reader-callback :reader reader-callback)
   (%socket :accessor %socket)
   (%reader-thread :accessor %reader-thread)
   (%writer-thread :accessor %writer-thread)
   (%writer-queue :accessor %writer-queue)))

(defmethod connect ((c vpn-connection))
  (setf (%socket c)
        (u:socket-connect (host c) (port c)
                          :protocol :datagram
                          :element-type '(unsigned-byte 8)))
  (setf (%writer-queue c) (lp.q:make-queue))
  (setf (%reader-thread c) (bt:make-thread (%reader-loop c)))
  (setf (%writer-thread c) (bt:make-thread (%writer-loop c))))

(defmethod disconnect ((c vpn-connection))
  (lp.q:push-queue 'stop (%writer-queue c))
  (bt:join-thread (%writer-thread c))
  (u:socket-close (%socket c))
  (bt:join-thread (%reader-thread c)))

(defun %reader-loop (c)
  (lambda ()
    (loop
      (multiple-value-bind (buffer size)
          (u:socket-receive (%socket c) nil 65507)
        (if (> size 0)
            (ignore-errors (funcall (reader-callback c) buffer size))
            (break))))))

(defun %writer-loop (c)
  (lambda ()
    (block writer
      (loop
        (let ((item (lp.q:pop-queue (%writer-queue c))))
          (when (eq item 'stop)
            (return-from writer))
          (u:socket-send (%socket c) item (length item)))))))

(defmethod send ((c vpn-connection) packet)
  (lp.q:push-queue packet (%writer-queue c)))
