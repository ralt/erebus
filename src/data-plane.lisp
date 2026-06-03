(in-package #:erebus)

;;; ---------------------------------------------------------------------------
;;; VPN-DATA-PLANE: the encapsulation-independent core shared by every
;;; concrete VPN client (OpenVPN static-key, IPsec/ESP, ...).
;;;
;;; Everything the embedded TCP/IP stack (socket.lisp), the HTTP proxy
;;; (http.lisp) and inbound port-forwarding lean on lives here: per-connection
;;; packet queues keyed by an inner-IP four-tuple, passive-open listeners,
;;; running exposures, ephemeral client-port bookkeeping, and the demux that
;;; routes a decapsulated inner IPv4 packet to the right queue.
;;;
;;; A subclass only has to: encapsulate/transmit an inner IP packet
;;; (SEND-PACKET), and, on receipt, decapsulate a wire packet down to an inner
;;; IPv4 buffer and hand it to %DISPATCH-INNER-IP.
;;; ---------------------------------------------------------------------------

(defclass vpn-data-plane ()
  (;; the underlying byte transport (a VPN-CONNECTION). Holds the
   ;; reader/writer threads and the ephemeral client-port set.
   (%vpn-connection :accessor %vpn-connection)
   ;; protocol -> (hash four-tuple-key -> packet queue). One sub-table per
   ;; inner protocol we demultiplex (ICMP, TCP).
   (%connections :accessor %connections)
   (%connections-lock :accessor %connections-lock)
   ;; ports we accept inbound connections on: dst-port -> accept queue.
   ;; guarded by %connections-lock.
   (%listeners :accessor %listeners)
   ;; running EXPOSURE handles, so DISCONNECT can tear them down.
   (%exposures :accessor %exposures :initform nil)
   ;; our address on the VPN, as a 32-bit integer.
   (%client-ip-address :accessor %client-ip-address)))

(defmethod initialize-instance :after ((c vpn-data-plane) &key)
  ;; Runs before any subclass :after method (least-specific-first), so the
  ;; shared tables exist by the time a subclass finishes wiring itself up.
  (setf (%connections c) (make-hash-table))
  ;; Initialize an empty hash table of connections for each protocol we
  ;; support so that we don't have to try doing that every time we make a
  ;; new connection.
  (dolist (protocol (list +icmp-protocol+ +tcp-protocol+))
    (setf (gethash protocol (%connections c)) (make-hash-table :test #'equal)))
  (setf (%listeners c) (make-hash-table))
  (setf (%connections-lock c) (bt:make-lock)))

;;; ---------------------------------------------------------------------------
;;; Listener / exposure / connection bookkeeping
;;; ---------------------------------------------------------------------------

(defmethod register-listener ((c vpn-data-plane) port queue)
  (bt:with-lock-held ((%connections-lock c))
    (setf (gethash port (%listeners c)) queue)))

(defmethod unregister-listener ((c vpn-data-plane) port)
  (bt:with-lock-held ((%connections-lock c))
    (remhash port (%listeners c))))

(defmethod register-exposure ((c vpn-data-plane) exposure)
  (bt:with-lock-held ((%connections-lock c))
    (push exposure (%exposures c))))

(defmethod unregister-exposure ((c vpn-data-plane) exposure)
  (bt:with-lock-held ((%connections-lock c))
    (setf (%exposures c) (remove exposure (%exposures c)))))

(defmethod remove-connection ((c vpn-data-plane) protocol key)
  (bt:with-lock-held ((%connections-lock c))
    (remhash key (gethash protocol (%connections c)))))

(defmethod find-free-client-port ((c vpn-data-plane))
  (find-free-client-port (%vpn-connection c)))

(defmethod release-client-port ((c vpn-data-plane) port)
  (release-client-port (%vpn-connection c) port))

(defmethod %ensure-connection-queue ((c vpn-data-plane) protocol key)
  "Create the per-connection packet queue for PROTOCOL/KEY lazily and keep
it for the lifetime of the connection. Recreating it on every send would
drop inbound packets that arrived between two of our sends, which breaks
multi-segment (fragmented) reads. Must be called with %CONNECTIONS-LOCK held."
  (unless (gethash key (gethash protocol (%connections c)))
    (setf (gethash key (gethash protocol (%connections c))) (lp.q:make-queue))))

;;; ---------------------------------------------------------------------------
;;; Receiving demultiplexed inner packets
;;; ---------------------------------------------------------------------------

(defmethod receive-packet ((c vpn-data-plane) protocol key)
  (let ((queue))
    (bt:with-lock-held ((%connections-lock c))
      (setf queue (gethash key (gethash protocol (%connections c)))))
    ;; make sure we wait for new item *without* holding the lock, it could
    ;; wait for a while and we want other packets to be processed in the
    ;; meantime.
    (let ((result (lp.q:pop-queue queue)))
      (when (eq (type-of result) 'condition)
        (error result))
      result)))

(defmethod poll-packet ((c vpn-data-plane) protocol key timeout)
  "Like RECEIVE-PACKET but waits at most TIMEOUT seconds, returning NIL if
no packet arrives in time."
  (let ((queue))
    (bt:with-lock-held ((%connections-lock c))
      (setf queue (gethash key (gethash protocol (%connections c)))))
    (multiple-value-bind (result presentp)
        (lp.q:try-pop-queue queue :timeout timeout)
      (when presentp
        (when (eq (type-of result) 'condition)
          (error result))
        result))))

(defun %error-callback (c)
  (lambda (condition)
    ;; just push the error to all the ongoing connections
    (maphash (lambda (protocol table)
               (declare (ignore protocol))
               (maphash (lambda (key queue)
                          (declare (ignore key))
                          (lp.q:push-queue condition queue))
                        table))
             (%connections c))))

;;; ---------------------------------------------------------------------------
;;; Inner-IP demux: route a decapsulated IPv4 packet to its connection queue.
;;; Identical regardless of the outer encapsulation (OpenVPN, ESP, ...), so it
;;; lives here and every client funnels its received packets through it.
;;; ---------------------------------------------------------------------------

(defmethod %dispatch-inner-ip ((c vpn-data-plane) type packet-header rest-stream)
  "TYPE/PACKET-HEADER/REST-STREAM come from a subclass having decapsulated
and parsed one inner packet (see %DESERIALIZE-PACKET): TYPE is :IP with an
IPV4-HEADER and a stream positioned at the L4 payload, or some other tag we
ignore here (e.g. :PING)."
  (when (eq type :ip)
    (let ((protocol (ipv4-header-protocol packet-header)))
      (cond
        ((= protocol +icmp-protocol+)
         (let* ((icmp-packet (bin:read-binary 'icmp-packet rest-stream))
                (key (icmp-packet-identifier icmp-packet)))
           (bt:with-lock-held ((%connections-lock c))
             (let ((queue (gethash key (gethash protocol (%connections c)))))
               (remhash key (gethash protocol (%connections c)))
               (lp.q:push-queue nil queue)))))

        ((= protocol +tcp-protocol+)
         (let* ((tcp-header (bin:read-binary 'tcp-header rest-stream))
                (tcp-header-length
                  (length
                   (fs:with-output-to-sequence (s)
                     (bin:write-binary tcp-header s))))
                (src-ip (ipv4-header-src-ip packet-header))
                (src-port (tcp-header-src-port tcp-header))
                (dst-ip (ipv4-header-dst-ip packet-header))
                (dst-port (tcp-header-dst-port tcp-header))
                (key (list dst-ip dst-port src-ip src-port)))
           (let ((queue)
                 (listener-queue)
                 (payload-size (- (ipv4-header-total-length packet-header)
                                  20 tcp-header-length)))
             ;; Look up the connection and, for a fresh inbound SYN, the
             ;; listener -- and create the per-connection queue -- all under
             ;; one lock, inside the single reader thread, so the follow-up
             ;; ACK/data cannot race ahead of the accept and get reset.
             (bt:with-lock-held ((%connections-lock c))
               (setf queue (gethash key (gethash protocol (%connections c))))
               (when (and (null queue)
                          (= 1 (tcp-header-syn tcp-header))
                          (= 0 (tcp-header-ack tcp-header)))
                 (setf listener-queue (gethash dst-port (%listeners c)))
                 (when listener-queue
                   (setf queue (lp.q:make-queue))
                   (setf (gethash key (gethash protocol (%connections c))) queue))))
             (cond
               ;; a peer is opening a connection to a port we listen on
               ;; (passive open): queue the SYN and hand the connection to
               ;; the listener to accept.
               (listener-queue
                (lp.q:push-queue (list tcp-header rest-stream payload-size) queue)
                (lp.q:push-queue (list dst-ip dst-port src-ip src-port)
                                 listener-queue))
               ;; a known connection: deliver a reset as a condition,
               ;; otherwise relay the segment.
               (queue
                (if (= 1 (tcp-header-rst tcp-header))
                    (lp.q:push-queue (make-condition 'econnreset) queue)
                    (lp.q:push-queue (list tcp-header rest-stream payload-size)
                                     queue)))
               ;; a stray RST for a connection that is already gone: nothing
               ;; to do.
               ((= 1 (tcp-header-rst tcp-header)) nil)
               ;; nothing is listening on this 4-tuple (e.g. a packet arriving
               ;; after we tore the connection down): reset the peer. The
               ;; RST's sequence number must be the ack number of the packet
               ;; we're replying to, otherwise the peer ignores it as out of
               ;; window.
               (t
                (send-packet c +tcp-protocol+ key
                             (%make-ipv4-tcp-packet dst-ip dst-port
                                                    src-ip src-port
                                                    :rst 1
                                                    :seqno (tcp-header-ackno tcp-header))
                             :skip-connection t))))))))))
