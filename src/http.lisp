(in-package #:erebus)

(defclass acceptor (h:acceptor)
  ((%client :initarg :client :accessor %client)))

(defmethod initialize-instance :after ((a acceptor) &key)
  ;; explicitly disable parsing of POST parameters as we want to
  ;; stream the body ourselves.
  (setf h:*methods-for-post-parameters* nil))

(defmethod h:acceptor-dispatch-request ((a acceptor) request)
  (multiple-value-bind (host port)
      (%parse-host-header request)
    (let* ((socket (socket-connect (%client a)
                                   :protocol :stream
                                   :host host
                                   :port port))
           (socket-stream (socket-stream socket)))
      ;; We have to manually input the headers, then the body can be
      ;; streamed. Once done, we have to do the same thing for the
      ;; request: manually output the headers, then stream the
      ;; response.
      (write-sequence (b:string-to-octets
                       (format nil
                               "~a ~a ~a~%"
                               (h:request-method request)
                               (h:request-uri request)
                               (h:server-protocol request)))
                      socket-stream)
      (dolist (header-pair (h:headers-in request))
        (write-sequence
         (b:string-to-octets (format nil "~@(~a~): ~a~%" (car header-pair) (cdr header-pair)))
         socket-stream))

      (write-sequence (make-array 1 :element-type 'octet :initial-contents '(10))
                      socket-stream)
      (finish-output socket-stream)
      ;; headers + double newline sent, now onto the body

      (uiop:copy-stream-to-stream (h:raw-post-data :request request :want-stream t)
                                  socket-stream)
      ;; acute readers will note that until %SOCKET-STREAM's
      ;; GS:STREAM-WRITE-SEQUENCE decides to flush by itself every now
      ;; and then, we're buffering the request body in-memory. TODO: don't?
      (finish-output socket-stream)

      ;; now it's time to read the response.
      (let ((buffer (make-array #xffff :element-type 'octet)))
        (read-sequence buffer socket-stream)

        (format t "~a" buffer)))))

(defun %parse-host-header (request)
  (let* ((host-header (cdr (assoc :host (h:headers-in request))))
         (parts (uiop:split-string host-header :separator ":"))
         (host (first parts))
         (port-str (or (second parts) "80")))
    (values host (parse-integer port-str))))
