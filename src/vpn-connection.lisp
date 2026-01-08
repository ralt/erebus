(in-package #:erebus)

(defclass vpn-connection ()
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (reader-callback :initarg :reader-callback :reader reader-callback)
   (error-callback :initarg :error-callback :reader error-callback)
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
  (setf (%reader-thread c) (bt:make-thread (%reader-loop c) :name "reader thread"))
  (setf (%writer-thread c) (bt:make-thread (%writer-loop c) :name "writer thread")))

(defmethod disconnect ((c vpn-connection))
  (lp.q:push-queue 'stop (%writer-queue c))
  (bt:join-thread (%writer-thread c))
  (u:socket-close (%socket c))
  (bt:interrupt-thread (%reader-thread c) #'identity t)
  (bt:join-thread (%reader-thread c)))

(defun %reader-loop (c)
  (lambda ()
    (block reader
      (loop
        (handler-case
            (multiple-value-bind (buffer size)
                (u:socket-receive (%socket c) nil 65507)
              (when (= size 0)
                (return-from reader))
              (handler-case
                  (funcall (reader-callback c) buffer size)
                (error (c)
                  (format t "error in reader callback: ~a~%" c))))
          (error (condition)
            ;; this one is expected, this is what we get when we
            ;; INTERRUPT-THREAD
            (unless (eq (type-of condition) 'u:bad-file-descriptor-error)
              (format t "error in reader loop: ~a~%" condition)
              (funcall (error-callback c) condition))
            (return-from reader)))))))

(defun %writer-loop (c)
  (lambda ()
    (block writer
      (loop
        (let ((item (lp.q:pop-queue (%writer-queue c))))
          (when (eq item 'stop)
            (return-from writer))
          (handler-case
              (u:socket-send (%socket c) item (length item))
            (error (c)
              (format t "error in writer loop: ~a~%" c))))))))

(defmethod send ((c vpn-connection) packet)
  (lp.q:push-queue packet (%writer-queue c)))
