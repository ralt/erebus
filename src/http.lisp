(in-package #:erebus)

(defclass acceptor (h:acceptor)
  ((%client :initarg :client :accessor %client)))

(defmethod initialize-instance :after ((a acceptor) &key)
  ;; explicitly disable parsing of POST parameters as we want to
  ;; stream the body ourselves.
  (setf h:*methods-for-post-parameters* nil))

;;; --- HTTPS CONNECT tunnelling support ---
;;;
;;; For a CONNECT request we actively open the VPN socket to the target,
;;; tell the client the tunnel is established, detach the socket from
;;; Hunchentoot, and hand both sockets to %RELAY (shared with inbound
;;; port-forwarding) to pump opaque bytes -- the TLS payload, which we
;;; never inspect -- both ways until either side closes.

(defvar *http-client-socket* nil
  "The client USOCKET of the connection currently being processed, captured
by the PROCESS-CONNECTION :AROUND method. %HANDLE-CONNECT needs the raw
socket (%RELAY polls it with WAIT-FOR-INPUT); Hunchentoot's request object
does not expose it.")

(defmethod h:process-connection :around ((a acceptor) socket)
  (let ((*http-client-socket* socket))
    (call-next-method)))

;; Hop-by-hop headers (RFC 7230 6.1) plus framing headers we regenerate
;; ourselves. These must not be blindly forwarded back to the client.
(defparameter +skipped-response-headers+
  '("connection" "keep-alive" "proxy-authenticate" "proxy-authorization"
    "te" "trailer" "transfer-encoding" "upgrade" "content-length"))

(defparameter +connect-established-response+
  (b:string-to-octets
   (format nil "HTTP/1.1 200 Connection Established~C~C~C~C"
           #\Return #\Linefeed #\Return #\Linefeed))
  "The CONNECT success status line. Built with explicit CR/LF: \"\\r\\n\" is
not an escape sequence in Common Lisp string literals.")

(defmethod h:acceptor-dispatch-request ((a acceptor) request)
  (if (eq (h:request-method request) :connect)
      (%handle-connect a request)
      (multiple-value-bind (host port)
          (%parse-host-header request)
        (let ((socket (socket-connect (%client a)
                                      :protocol :stream
                                      :host (%resolve-hostname host)
                                      :port port)))
          (unwind-protect
               (let ((socket-stream (socket-stream socket)))
                 (%forward-request request socket-stream)
                 (%forward-response socket-stream))
            (socket-close socket))))))

(defun %handle-connect (a request)
  "Tunnel a CONNECT request: open the VPN socket to the requested
authority, acknowledge the tunnel, and relay opaque bytes between the
client and the target until either side closes."
  (multiple-value-bind (host port)
      (%parse-authority (h:request-uri request))
    (let ((vpn-socket (socket-connect (%client a)
                                      :protocol :stream
                                      :host (%resolve-hostname host)
                                      :port port))
          (client-socket *http-client-socket*))
      (unwind-protect
           (let ((client-stream (u:socket-stream client-socket)))
             (write-sequence +connect-established-response+ client-stream)
             (finish-output client-stream)
             ;; suppress Hunchentoot's own response and take ownership of
             ;; the socket; from here we are just an opaque byte pump.
             (setf hunchentoot::*headers-sent* t)
             (h:detach-socket a)
             (%relay vpn-socket client-socket))
        (ignore-errors (socket-close vpn-socket))
        (ignore-errors (u:socket-close client-socket))))))

(defun %parse-authority (authority)
  "Split an authority-form request target (\"host:port\") into HOST and PORT."
  (let ((colon (position #\: authority)))
    (values (subseq authority 0 colon)
            (parse-integer (subseq authority (1+ colon))))))

(defun %forward-request (request socket-stream)
  "Write REQUEST (status line, headers and body) to the VPN-side socket."
  ;; We have to manually input the headers, then the body can be
  ;; streamed.
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
  ;; headers + blank line sent, now onto the body.

  ;; We read the body as a plain octet vector rather than as a stream:
  ;; requesting it :want-stream re-wraps hunchentoot's (shared) client
  ;; socket stream, which then breaks the response output path further
  ;; down. The whole body is buffered in memory either way for now.
  (let ((body (h:raw-post-data :request request :force-binary t)))
    (when (and body (plusp (length body)))
      (write-sequence body socket-stream)))

  (finish-output socket-stream))

(defun %forward-response (socket-stream)
  "Read an HTTP response from the VPN-side socket and relay it to the
local client through hunchentoot. The body is read across as many TCP
segments as needed, so responses larger than a single segment work."
  (multiple-value-bind (status-code headers)
      (%read-response-head socket-stream)
    (setf (h:return-code*) status-code)
    (dolist (header headers)
      (unless (member (car header) +skipped-response-headers+ :test #'string=)
        (setf (h:header-out (car header)) (cdr header))))
    (%read-response-body socket-stream headers)))

(defun %read-line-crlf (stream)
  "Read one CRLF- (or LF-) terminated line of bytes from STREAM and
return it as a latin-1 string without the terminator. Returns NIL at end
of stream before any byte was read."
  (let ((bytes (make-array 16 :element-type 'octet :adjustable t :fill-pointer 0)))
    (loop
      (let ((b (read-byte stream nil :eof)))
        (cond
          ((eq b :eof)
           (return (when (plusp (fill-pointer bytes))
                     (%octets->string bytes))))
          ((= b 10)                     ; LF: end of line
           (when (and (plusp (fill-pointer bytes))
                      (= (aref bytes (1- (fill-pointer bytes))) 13))
             (decf (fill-pointer bytes))) ; drop trailing CR
           (return (%octets->string bytes)))
          (t (vector-push-extend b bytes)))))))

(defun %octets->string (bytes)
  (b:octets-to-string (coerce bytes 'octet-vector) :encoding :latin-1))

(defun %read-response-head (socket-stream)
  "Read the status line and headers. Returns (values status-code headers)
where HEADERS is an alist of lower-cased name to value."
  (let* ((status-line (%read-line-crlf socket-stream))
         (parts (uiop:split-string status-line :separator " "))
         (status-code (parse-integer (second parts)))
         (headers '()))
    (loop for line = (%read-line-crlf socket-stream)
          while (and line (string/= line ""))
          do (let* ((colon (position #\: line))
                    (name (string-downcase (string-trim " " (subseq line 0 colon))))
                    (value (string-trim " " (subseq line (1+ colon)))))
               (push (cons name value) headers)))
    (values status-code (nreverse headers))))

(defun %read-response-body (socket-stream headers)
  "Stream the response body back to the local client, choosing the
framing from the response headers: explicit Content-Length, chunked
transfer-encoding, or read-until-close."
  (let ((content-length (cdr (assoc "content-length" headers :test #'string=)))
        (transfer-encoding (cdr (assoc "transfer-encoding" headers :test #'string=))))
    ;; when the upstream length is known, hand it to hunchentoot before
    ;; sending headers so the local client gets Content-Length framing
    ;; (and clean keep-alive) instead of forcing chunked output.
    (when content-length
      (setf (h:content-length*) (parse-integer content-length)))
    (let ((out (h:send-headers)))
      (cond
        (content-length
         (%copy-n-bytes socket-stream out (parse-integer content-length)))
        ((and transfer-encoding (search "chunked" transfer-encoding))
         (%copy-chunked socket-stream out))
        (t
         (%copy-until-eof socket-stream out)))
      (finish-output out))))

(defun %copy-n-bytes (in out n)
  (let ((buffer (make-array (min n #x4000) :element-type 'octet))
        (remaining n))
    (loop while (plusp remaining)
          do (let ((nb (read-sequence buffer in :end (min remaining (length buffer)))))
               (when (zerop nb)
                 (return))             ; peer closed early
               (write-sequence buffer out :end nb)
               (decf remaining nb)))))

(defun %copy-until-eof (in out)
  (let ((buffer (make-array #x4000 :element-type 'octet)))
    (loop for nb = (read-sequence buffer in)
          while (plusp nb)
          do (write-sequence buffer out :end nb))))

(defun %copy-chunked (in out)
  (loop
    (let* ((size-line (%read-line-crlf in))
           (size (and size-line (parse-integer size-line :radix 16 :junk-allowed t))))
      (when (or (null size) (zerop size))
        ;; consume the trailing CRLF / trailers up to the blank line.
        (loop for line = (%read-line-crlf in)
              while (and line (string/= line "")))
        (return))
      (%copy-n-bytes in out size)
      (%read-line-crlf in))))          ; consume CRLF after the chunk data

(defun %parse-host-header (request)
  (let* ((host-header (cdr (assoc :host (h:headers-in request))))
         (parts (uiop:split-string host-header :separator ":"))
         (host (first parts))
         (port-str (or (second parts) "80")))
    (values host (parse-integer port-str))))

(defun %resolve-hostname (host)
  (format nil "~{~a~^.~}" (coerce (u:get-host-by-name host) 'list)))
