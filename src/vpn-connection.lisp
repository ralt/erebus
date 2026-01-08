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
   (%writer-queue :accessor %writer-queue)))

(defmethod connect ((c vpn-connection))
  (setf (%socket c)
        (u:socket-connect (host c) (port c)
                          :protocol (protocol c)
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
            (cond
              ((eq (protocol c) :datagram)
               (multiple-value-bind (buffer size)
                   (u:socket-receive (%socket c) nil #xffff)
                 (handler-case
                     (funcall (reader-callback c) buffer size)
                   (error (condition)
                     (format t "error in reader callback: ~a~%" condition)
                     (when (eq (type-of condition) 'u:socket-condition)
                       (funcall (error-callback c) condition)
                       (return-from reader))))))
              ((eq (protocol c) :stream)
               (progn
                 (u:wait-for-input (%socket c))
                 (handler-case
                     (funcall (reader-callback c) (u:socket-stream (%socket c)))
                   (end-of-file (condition)
                     (format t "got eof, leaving reader loop~%")
                     (funcall (error-callback c) condition)
                     (return-from reader))
                   (error (condition)
                     (format t "error in reader callback: ~a~%" condition)
                     (when (eq (type-of condition) 'u:socket-condition)
                       (funcall (error-callback c) condition)
                       (return-from reader)))))))
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
