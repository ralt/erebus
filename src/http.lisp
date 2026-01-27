(in-package #:erebus)

(defclass acceptor (h:acceptor)
  ((%client :initarg :client :accessor %client)))

(defmethod h:acceptor-dispatch-request ((a acceptor) request)
  (let ((response-stream (h:send-headers))
        (buf (make-array 3 :element-type 'octet :initial-contents '(65 66 10))))
    (fs:with-input-from-sequence (s buf)
      (uiop:copy-stream-to-stream s response-stream :element-type 'octet))))
