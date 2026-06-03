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

(defparameter *relay-poll-timeout* 0.05
  "Seconds we wait on a connection's packet queue per poll when we are
driving it ourselves (the relay's select loop, and SOCKET-CLOSE's bounded
drain). The relay is single-threaded by design, so it polls each side in
turn; this trades a little latency for a simple, easy-to-follow design
rather than chasing throughput.")

(defun %fill-read-buffer-poll (s timeout)
  "Like %FILL-READ-BUFFER but waits at most TIMEOUT seconds for a segment.
Returns :DATA when payload was buffered, :EOF when the peer has closed, or
:NONE when nothing usable arrived in time."
  (let* ((socket (%socket s))
         (client (client socket))
         (packet (poll-packet client +tcp-protocol+ (%key socket) timeout)))
    (if (null packet)
        :none
        (destructuring-bind (tcp-header rest-stream size) packet
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
                 :data))
              (fin
               (%next-ackno socket 1)
               (%send-ack socket)
               (setf (%eof socket) t)
               :eof)
              ;; a bare ACK: nothing to deliver this tick.
              (t :none)))))))

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
   ;; a passive socket is one the peer opened (an inbound connection we
   ;; accepted), as opposed to one we opened ourselves. The two differ
   ;; only in how they reach the ESTABLISHED state (active vs passive
   ;; open) and in port bookkeeping at close time.
   (%passive :initarg :passive :reader %passive :initform nil)
   (stream :reader socket-stream :accessor %stream)
   ;; for a passive socket the four-tuple is known up front (it comes
   ;; from the inbound SYN), so allow it to be supplied as initargs.
   (%src-ip :initarg :src-ip :accessor %src-ip)
   (%src-port :initarg :src-port :accessor %src-port)
   (%dst-ip :initarg :dst-ip :accessor %dst-ip)
   (%dst-port :initarg :dst-port :accessor %dst-port)
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
  (if (%passive s)
      (%passive-open s)
      (%active-open s)))

(defun %active-open (s)
  "Open a connection we initiate: send a SYN, expect the SYN-ACK, and ACK
it (the classic three-way handshake from the client side)."
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

(defun %passive-open (s)
  "Complete a connection a peer opened to us. The reader has already
queued the inbound SYN for our four-tuple (set from initargs); reply with
a SYN-ACK and wait for the final ACK. Mirrors %ACTIVE-OPEN's sequence
arithmetic from the server side."
  (setf (%seqno s) (random #xffffffff))
  (let ((client (client s))
        (key (%key s)))
    ;; the peer's SYN
    (destructuring-bind (tcp-header rest-stream size)
        (receive-packet client +tcp-protocol+ key)
      (declare (ignore rest-stream size))
      (assert (= 1 (tcp-header-syn tcp-header)))
      (assert (= 0 (tcp-header-ack tcp-header)))
      (setf (%ackno s) (tcp-header-seqno tcp-header)))

    ;; syn-ack
    (send-packet client +tcp-protocol+ key
                 (%make-ipv4-tcp-packet (%src-ip s) (%src-port s)
                                        (%dst-ip s) (%dst-port s)
                                        :syn 1
                                        :ack 1
                                        :seqno (%next-seqno s)
                                        :ackno (%next-ackno s)))

    ;; expose the stream now, so that a final ACK piggybacking the first
    ;; data segment (a single ACK+PSH packet) is not lost.
    (setf (%stream s) (make-instance '%socket-stream :socket s))

    ;; the peer's final ACK (may already carry data)
    (let ((stream (socket-stream s)))
      (destructuring-bind (tcp-header rest-stream size)
          (receive-packet client +tcp-protocol+ key)
        (assert (= 1 (tcp-header-ack tcp-header)))
        (assert (= (mod (1+ (%seqno s)) +max-32-bytes+) (tcp-header-ackno tcp-header)))
        ;; our SYN-ACK consumed one sequence number.
        (%next-seqno s)
        (let ((fin (= 1 (tcp-header-fin tcp-header))))
          (when (> size 0)
            (let ((buffer (make-array size :element-type 'octet)))
              (read-sequence buffer rest-stream)
              (%next-ackno s (+ size (if fin 1 0)))
              (%send-ack s)
              (%stash-read stream buffer)))
          (when (and fin (zerop size))
            (%next-ackno s 1)
            (%send-ack s)
            (setf (%eof s) t)))))))

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
          ;; remaining payload. Bounded: a peer that holds the connection
          ;; open (e.g. an HTTP keep-alive server that never FINs) must not
          ;; block us forever -- we have already sent our FIN, which is the
          ;; part that matters for an orderly close.
          (loop repeat 40                       ; ~2s at *relay-poll-timeout*
                until (%eof s)
                do (%fill-read-buffer-poll stream *relay-poll-timeout*)))
      ;; a reset (or the reader thread going away) just means the
      ;; connection is already gone; nothing left to clean up on the wire.
      (econnreset () nil)
      (error () nil))
    (remove-connection client +tcp-protocol+ (%key s))
    ;; a passive socket's local port is the fixed exposed port, not an
    ;; ephemeral one we allocated, so there is nothing to release.
    (unless (%passive s)
      (release-client-port client (%src-port s)))))

;;; ---------------------------------------------------------------------------
;;; Listening for inbound connections (the server side of the stack)
;;; ---------------------------------------------------------------------------

(defclass server-socket ()
  ((client :initarg :client :reader client)
   (port :initarg :port :reader port)
   ;; the reader pushes a descriptor for each inbound SYN onto this queue;
   ;; SOCKET-ACCEPT pops them and completes the handshakes.
   (%queue :initarg :queue :reader %queue)))

(defun socket-listen (client &key port)
  "Listen for inbound VPN connections destined to PORT. Returns a
SERVER-SOCKET whose SOCKET-ACCEPT yields established connections."
  (let ((queue (lp.q:make-queue)))
    (register-listener client port queue)
    (make-instance 'server-socket :client client :port port :queue queue)))

(defmethod socket-accept ((s server-socket))
  "Block until a peer opens a connection to the listening port, complete
the passive open, and return the established CLIENT-SOCKET."
  (destructuring-bind (local-ip local-port peer-ip peer-port)
      (lp.q:pop-queue (%queue s))
    (make-instance 'client-socket
                   :client (client s)
                   :protocol :stream
                   :passive t
                   :src-ip local-ip
                   :src-port local-port
                   :dst-ip peer-ip
                   :dst-port peer-port)))

(defmethod socket-unlisten ((s server-socket))
  "Stop accepting new connections on this listener."
  (unregister-listener (client s) (port s)))

;;; ---------------------------------------------------------------------------
;;; Exposing a local service: accept inbound connections and relay each to
;;; a local OS socket. Deliberately single-threaded per connection (see
;;; %RELAY) so we never read and write the same VPN connection at once.
;;; ---------------------------------------------------------------------------

(defclass exposure ()
  ((client :initarg :client :reader client)
   (%server-socket :initarg :server-socket :reader %server-socket)
   (host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (%accept-thread :accessor %accept-thread)))

(defun expose (client &key vpn-port host port)
  "Expose a local TCP service to VPN peers: accept inbound connections on
VPN-PORT and relay each to HOST:PORT (a socket on this machine). Returns
an EXPOSURE handle; stop it with UNEXPOSE."
  (let* ((server (socket-listen client :port vpn-port))
         (exposure (make-instance 'exposure
                                  :client client
                                  :server-socket server
                                  :host host
                                  :port port)))
    (setf (%accept-thread exposure)
          (bt:make-thread (%accept-loop exposure)
                          :name (format nil "expose accept ~a" vpn-port)))
    (register-exposure client exposure)
    exposure))

(defun unexpose (exposure)
  "Stop an EXPOSURE: stop listening and tear down its accept loop."
  (socket-unlisten (%server-socket exposure))
  (ignore-errors (bt:destroy-thread (%accept-thread exposure)))
  (unregister-exposure (client exposure) exposure))

(defun %accept-loop (exposure)
  (lambda ()
    (loop
      (let ((vpn-socket (socket-accept (%server-socket exposure))))
        ;; one thread per accepted connection; the relay inside is itself
        ;; single-threaded.
        (bt:make-thread
         (lambda () (%handle-exposed-connection exposure vpn-socket))
         :name "expose connection")))))

(defun %handle-exposed-connection (exposure vpn-socket)
  (handler-case
      (let ((os-socket (u:socket-connect (host exposure) (port exposure)
                                         :protocol :stream
                                         :element-type 'octet)))
        (unwind-protect
             (%relay vpn-socket os-socket)
          (ignore-errors (u:socket-close os-socket))))
    (error (c)
      (format t "error handling exposed connection: ~a~%" c)))
  (ignore-errors (socket-close vpn-socket)))

(defun %relay (vpn-socket os-socket)
  "Pump bytes both ways between an established VPN socket and a local OS
socket until either side closes. Single-threaded on purpose: the
userspace TCP stack uses one packet queue per connection, so reading and
writing the same connection from two threads would race. We instead poll
each side in turn."
  (let ((vpn-stream (socket-stream vpn-socket))
        (os-stream (u:socket-stream os-socket))
        (buffer (make-array #x4000 :element-type 'octet)))
    (loop
      ;; 1. flush peer bytes already buffered on the VPN stream (e.g. data
      ;;    that piggybacked on an ACK while we were writing).
      (unless (%read-buffer-empty-p vpn-stream)
        (%flush-vpn-read-buffer vpn-stream os-stream))
      ;; 2. local -> peer
      (when (u:wait-for-input os-socket :timeout 0 :ready-only t)
        (let ((n (%drain-ready-input os-stream buffer)))
          (when (eq n :eof) (return))   ; local closed its side
          (write-sequence buffer vpn-stream :end n)
          (finish-output vpn-stream)))
      ;; 3. peer -> local (block briefly so we don't busy-spin)
      (when (and (%read-buffer-empty-p vpn-stream) (not (%eof vpn-socket)))
        (case (%fill-read-buffer-poll vpn-stream *relay-poll-timeout*)
          (:data (%flush-vpn-read-buffer vpn-stream os-stream))
          (:eof  (return))))           ; peer closed its side
      (when (%eof vpn-socket) (return)))))

(defun %flush-vpn-read-buffer (vpn-stream os-stream)
  "Write the unconsumed portion of VPN-STREAM's read buffer to OS-STREAM."
  (write-sequence (%read-buffer vpn-stream) os-stream
                  :start (%read-pos vpn-stream)
                  :end (length (%read-buffer vpn-stream)))
  (finish-output os-stream)
  (setf (%read-pos vpn-stream) (length (%read-buffer vpn-stream))))

(defun %drain-ready-input (stream buffer)
  "STREAM is known to have input ready. Read the bytes available right now
\(without blocking for a full buffer) into BUFFER, up to its length.
Returns the byte count, or :EOF at end of stream."
  (let ((first (read-byte stream nil :eof)))
    (if (eq first :eof)
        :eof
        (progn
          (setf (aref buffer 0) first)
          (let ((n 1))
            (loop while (and (< n (length buffer)) (listen stream))
                  for b = (read-byte stream nil :eof)
                  until (eq b :eof)
                  do (setf (aref buffer n) b)
                     (incf n))
            n)))))

