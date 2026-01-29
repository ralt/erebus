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
                                   :host (%resolve-hostname host)
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

      (let ((request-body-stream (h:raw-post-data :request request
                                                  :want-stream t
                                                  :force-binary t)))
        (when (fs:flexi-stream-bound request-body-stream)
          (uiop:copy-stream-to-stream request-body-stream socket-stream)))

      ;; acute readers will note that until %SOCKET-STREAM's
      ;; GS:STREAM-WRITE-SEQUENCE decides to flush by itself every now
      ;; and then, we're buffering the request body in-memory. TODO: don't?
      (finish-output socket-stream)

      ;; now it's time to read the response.
      (let* ((buffer (make-array #xffff :element-type 'octet))
             (nb (read-sequence buffer socket-stream)))
        ;; acute readers will note that this only works for responses
        ;; below ~1.5k bytes until TCP fragmentation is supported.
        ;; I'm not quite sure if I should care about staying in
        ;; streaming mode in case the body is large, or not care
        ;; because fragmentation is going to take care of it for
        ;; me. For now, just going to not care.
        (fs:with-input-from-sequence (in buffer :end nb)
          (setq in (fs:make-flexi-stream in :external-format :utf-8))
          (let* ((status-line (read-line in))
                 (parts (uiop:split-string status-line :separator " ")))
            (setf (h:return-code*) (parse-integer (second parts))))

          (block headers
            (loop
              (let ((line (uiop:stripln (read-line in))))
                (when (string= line "")
                  (return-from headers))
                (let* ((parts (ppcre:split ":" line :limit 2))
                       (header-name (string-downcase (first parts)))
                       (header-value (second parts)))
                  (cond ((string= header-name "content-length")
                         (setf (h:header-out header-name) (parse-integer header-value)))
                        (t (setf (h:header-out header-name) header-value)))))))

          (uiop:slurp-stream-string in))))))

(defun %parse-host-header (request)
  (let* ((host-header (cdr (assoc :host (h:headers-in request))))
         (parts (uiop:split-string host-header :separator ":"))
         (host (first parts))
         (port-str (or (second parts) "80")))
    (values host (parse-integer port-str))))

(defun %resolve-hostname (host)
  (format nil "~{~a~^.~}" (coerce (u:get-host-by-name host) 'list)))
