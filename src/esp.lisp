(in-package #:erebus)

;;; ---------------------------------------------------------------------------
;;; ESP data plane (RFC 4303) + the IPsec/IKEv2 client.
;;;
;;; IPSEC-CLIENT is a VPN-DATA-PLANE, so the embedded TCP/IP stack, the HTTP
;;; proxy and inbound port-forwarding work over it unchanged: it just has to
;;; encapsulate an inner IPv4 packet in ESP (SEND-PACKET) and, on receipt,
;;; decrypt an ESP packet back to an inner IPv4 buffer and hand it to
;;; %DISPATCH-INNER-IP.
;;;
;;; We are rootless and cannot open raw IP-proto-50 sockets, so everything --
;;; both IKE and ESP -- rides inside UDP (NAT-T). IKE_SA_INIT happens on
;;; port 500; we force NAT detection so the responder floats to port 4500,
;;; where IKE messages carry a 4-byte non-ESP marker and ESP packets are bare
;;; (their non-zero SPI distinguishes them).
;;; ---------------------------------------------------------------------------

(defclass ipsec-client (vpn-data-plane)
  ((host :initarg :host :reader host)
   (ike-port :initarg :ike-port :reader ike-port :initform 500)
   ;; the NAT-T / ESP port. Configurable because docker publishes the
   ;; container's 4500 on an arbitrary host port during tests.
   (natt-port :initarg :natt-port :reader natt-port :initform 4500)
   (psk :initarg :psk :reader psk)
   (local-id :initarg :local-id :reader local-id :initform "erebus")
   ;; IKE SA state, filled in by the handshake.
   (%spii :accessor %spii)
   (%spir :accessor %spir)
   (%ike-keys :accessor %ike-keys)        ; the SK_* plist, for INFORMATIONAL
   ;; message id for initiator-initiated exchanges after IKE_AUTH (which is 1).
   (%ike-message-id :accessor %ike-message-id :initform 2)
   (%esp-spi-out :accessor %esp-spi-out)   ; responder's ESP SPI (we send to it)
   (%esp-spi-in :accessor %esp-spi-in)     ; our ESP SPI (responder sends to it)
   (%esp-keys :accessor %esp-keys)
   (%esp-seq :accessor %esp-seq :initform 0)
   (%esp-seq-lock :accessor %esp-seq-lock)
   (%udp-socket :accessor %udp-socket)
   (%keepalive-thread :accessor %keepalive-thread)))

;;; --- synchronous UDP request/response used during the handshake ------------

(defun %udp-send (socket bytes)
  (u:socket-send socket bytes (length bytes)))

(defun %udp-recv (socket &optional (timeout 5))
  "Receive one datagram, waiting at most TIMEOUT seconds. Signals on timeout."
  (unless (u:wait-for-input socket :timeout timeout :ready-only t)
    (error "IKE: timed out waiting for a response from the server"))
  (multiple-value-bind (buffer size) (u:socket-receive socket nil #xffff)
    (subseq buffer 0 size)))

(defconstant +non-esp-marker+ 4
  "On port 4500, IKE messages are prefixed with this many zero bytes so they
can be told apart from ESP packets (which begin with a non-zero SPI).")

;;; --- ESP packet encrypt / decrypt ------------------------------------------

(defun %esp-encrypt (c inner-ip-bytes)
  "Wrap INNER-IP-BYTES (a full inner IPv4 packet) in an ESP packet:
SPI | Seq | IV | enc{ payload | pad | pad-len | next-hdr=IPv4 } | ICV."
  (let* ((keys (%esp-keys c))
         (enc-key (getf keys :encr-out))
         (int-key (getf keys :integ-out))
         (seq (bt:with-lock-held ((%esp-seq-lock c)) (incf (%esp-seq c))))
         (iv (ic:random-data 16))
         ;; pad so payload+pad-len-byte+next-hdr-byte is a multiple of 16.
         (padlen (mod (- 16 (mod (+ (length inner-ip-bytes) 2) 16)) 16))
         (to-encrypt (%octets inner-ip-bytes
                              (make-array padlen :element-type 'octet)
                              padlen
                              4))                   ; next-header = 4 (inner IPv4)
         (cipher (ic:make-cipher :aes :mode :cbc :key enc-key :initialization-vector iv))
         (ct (make-array (length to-encrypt) :element-type 'octet)))
    (ic:encrypt cipher to-encrypt ct)
    (let* ((esp-header (%octets (%esp-spi-out c) (int->bytes seq 4)))
           (mac-data (%octets esp-header iv ct))
           (icv (subseq (%prf int-key mac-data) 0 16)))
      (%octets mac-data icv))))

(defun %esp-decrypt (c buffer)
  "Verify and decrypt an inbound ESP packet. Returns (values inner-ip-bytes
next-header), or NIL if the ICV check fails."
  (let* ((keys (%esp-keys c))
         (enc-key (getf keys :encr-in))
         (int-key (getf keys :integ-in))
         (size (length buffer))
         (icv (subseq buffer (- size 16) size))
         (mac-data (subseq buffer 0 (- size 16)))
         (expected (subseq (%prf int-key mac-data) 0 16)))
    (when (ic:constant-time-equal icv expected)
      (let* ((iv (subseq buffer 8 24))
             (ct (subseq buffer 24 (- size 16)))
             (cipher (ic:make-cipher :aes :mode :cbc :key enc-key
                                          :initialization-vector iv))
             (pt (make-array (length ct) :element-type 'octet)))
        (ic:decrypt cipher ct pt)
        (let ((padlen (aref pt (- (length pt) 2)))
              (next-header (aref pt (- (length pt) 1))))
          (values (subseq pt 0 (- (length pt) padlen 2)) next-header))))))

;;; --- the IKEv2 handshake (synchronous, runs inside CONNECT) ----------------

(defun %resolve-ipv4 (host)
  "HOST as a 32-bit integer (resolves a name if needed)."
  (handler-case (string-ipv4-address-to-integer host)
    (error ()
      (let ((addr (u:get-host-by-name host)))
        (+ (ash (elt addr 0) 24) (ash (elt addr 1) 16)
           (ash (elt addr 2) 8) (elt addr 3))))))

(defun %ike-sa-init (c)
  "Run the IKE_SA_INIT exchange on a fresh UDP socket to the server's IKE
port. Returns a plist of everything the IKE_AUTH step needs."
  (let* ((spii (ic:random-data 8))
         (spir-zero (make-array 8 :element-type 'octet))
         (server-ip (%resolve-ipv4 (host c)))
         (socket (u:socket-connect (host c) (ike-port c)
                                   :protocol :datagram :element-type 'octet)))
    (multiple-value-bind (dh-private dh-public) (make-dh-keypair)
      (let* ((ni (ic:random-data 32))
             ;; force NAT detection: a source hash the responder can't match
             ;; (over 0.0.0.0:0) makes it treat us as NATed and float to 4500.
             (nat-src (nat-detection-hash spii spir-zero 0 0))
             (nat-dst (nat-detection-hash spii spir-zero server-ip (ike-port c)))
             (request
               (build-ike-message
                spii spir-zero +ex-sa-init+ +flag-initiator+ 0
                (list (cons +pl-sa+ (build-sa-ike))
                      (cons +pl-ke+ (build-ke dh-public))
                      (cons +pl-nonce+ (build-nonce ni))
                      (cons +pl-notify+
                            (build-notify 0 +nat-detection-source-ip+ #() nat-src))
                      (cons +pl-notify+
                            (build-notify 0 +nat-detection-destination-ip+ #() nat-dst))))))
        (%udp-send socket request)
        (let* ((response (%udp-recv socket))
               (header (parse-ike-header response))
               (payloads (parse-payloads (ike-header-next-payload header)
                                         (subseq response 28)))
               (err (error-notify-type payloads)))
          (when err
            (error "IKE_SA_INIT rejected by server (notify type ~a)" err))
          (let* ((ke (payload-body +pl-ke+ payloads))
                 (peer-public (subseq ke 4))          ; skip group(2)+reserved(2)
                 (nr (payload-body +pl-nonce+ payloads))
                 (spir (ike-header-spir header))
                 (shared (dh-shared-secret dh-private peer-public))
                 (keys (derive-ike-keys ni nr shared spii spir)))
            (u:socket-close socket)
            (list :spii spii :spir spir :ni ni :nr nr :keys keys
                  :real-message-1 request :real-message-2 response)))))))

(defun %ike-auth (c init)
  "Run the IKE_AUTH exchange on a fresh UDP socket floated to port 4500.
Verifies the responder's AUTH and returns the established socket plus the
CHILD_SA parameters."
  (let* ((spii (getf init :spii))
         (spir (getf init :spir))
         (keys (getf init :keys))
         (sk-ei (getf keys :sk-ei)) (sk-ai (getf keys :sk-ai))
         (sk-er (getf keys :sk-er)) (sk-ar (getf keys :sk-ar))
         (sk-pi (getf keys :sk-pi)) (sk-pr (getf keys :sk-pr))
         (psk-bytes (b:string-to-octets (psk c)))
         (esp-spi-in (ic:random-data 4))
         (idi (build-idi +id-fqdn+ (b:string-to-octets (local-id c))))
         ;; InitiatorSignedOctets = RealMessage1 | Nr | prf(SK_pi, IDi')
         (signed (initiator-signed-octets (getf init :real-message-1)
                                          (getf init :nr) sk-pi idi))
         (auth (compute-psk-auth psk-bytes signed))
         (request
           (build-encrypted-ike-message
            spii spir +ex-auth+ +flag-initiator+ 1
            (list (cons +pl-idi+ idi)
                  (cons +pl-auth+ (build-auth auth))
                  (cons +pl-sa+ (build-sa-esp esp-spi-in))
                  (cons +pl-tsi+ (build-ts-full))
                  (cons +pl-tsr+ (build-ts-full))
                  (cons +pl-cp+ (build-cp-request-ip4)))
            sk-ei sk-ai))
         (socket (u:socket-connect (host c) (natt-port c)
                                   :protocol :datagram :element-type 'octet))
         (marker (make-array +non-esp-marker+ :element-type 'octet)))
    (%udp-send socket (%octets marker request))
    (let* ((raw (%udp-recv socket))
           (response (subseq raw +non-esp-marker+)))   ; strip the non-ESP marker
      (multiple-value-bind (header payloads)
          (parse-encrypted-ike-message response sk-er sk-ar)
        (declare (ignore header))
        (let ((err (error-notify-type payloads)))
          (when err
            (u:socket-close socket)
            (error "IKE_AUTH rejected by server (notify type ~a)" err)))
        ;; verify the responder's AUTH
        (let* ((idr (payload-body +pl-idr+ payloads))
               (auth-payload (payload-body +pl-auth+ payloads))
               (their-auth (subseq auth-payload 4))     ; skip method(1)+reserved(3)
               (signed-r (responder-signed-octets (getf init :real-message-2)
                                                  (getf init :ni) sk-pr idr))
               (expected (compute-psk-auth psk-bytes signed-r)))
          (unless (and idr (ic:constant-time-equal their-auth expected))
            (u:socket-close socket)
            (error "IKE_AUTH: responder authentication failed"))
          ;; CHILD_SA: responder's ESP SPI, and our assigned inner IPv4.
          (let* ((sa (payload-body +pl-sa+ payloads))
                 (esp-spi-out (sa-first-spi sa))
                 (cp (payload-body +pl-cp+ payloads))
                 (assigned-ip (and cp (cp-internal-ip4 cp))))
            (unless esp-spi-out
              (u:socket-close socket)
              (error "IKE_AUTH: responder did not return an ESP SPI"))
            (list :socket socket
                  :esp-spi-in esp-spi-in
                  :esp-spi-out esp-spi-out
                  :assigned-ip assigned-ip
                  :esp-keys (derive-esp-keys (getf keys :sk-d)
                                             (getf init :ni) (getf init :nr)))))))))

;;; --- CONNECT / DISCONNECT ---------------------------------------------------

(defmethod connect ((c ipsec-client))
  (setf (%esp-seq-lock c) (bt:make-lock "esp-seq"))
  (let* ((init (%ike-sa-init c))
         (auth (%ike-auth c init)))
    (setf (%spii c) (getf init :spii)
          (%spir c) (getf init :spir)
          (%ike-keys c) (getf init :keys)
          (%esp-spi-in c) (getf auth :esp-spi-in)
          (%esp-spi-out c) (getf auth :esp-spi-out)
          (%esp-keys c) (getf auth :esp-keys)
          (%udp-socket c) (getf auth :socket))
    (let ((assigned (getf auth :assigned-ip)))
      (unless assigned
        (error "IKEv2: server did not assign an internal IPv4 address"))
      (setf (%client-ip-address c) assigned))
    ;; hand the floated 4500 socket to a VPN-CONNECTION for the data phase:
    ;; it gives us the reader/writer threads and ephemeral-port bookkeeping.
    (setf (%vpn-connection c)
          (make-instance 'vpn-connection
                         :protocol :datagram
                         :host (host c) :port (natt-port c)
                         :socket (%udp-socket c)
                         :reader-callback (%ipsec-reader-callback c)
                         :error-callback (%error-callback c)))
    (connect (%vpn-connection c))
    (setf (%keepalive-thread c)
          (bt:make-thread (%nat-keepalive-loop c) :name "ipsec nat-keepalive"))))

(defun %ike-delete (c)
  "Send an INFORMATIONAL exchange deleting the IKE SA, so the server tears
down the tunnel and releases our pool lease. Best-effort and fire-and-forget:
it rides the data-phase 4500 socket (hence the non-ESP marker) and we don't
wait for the response."
  (let* ((keys (%ike-keys c))
         (msgid (%ike-message-id c))
         (msg (build-encrypted-ike-message
               (%spii c) (%spir c) +ex-informational+ +flag-initiator+ msgid
               (list (cons +pl-delete+ (build-delete-ike)))
               (getf keys :sk-ei) (getf keys :sk-ai)))
         (marker (make-array +non-esp-marker+ :element-type 'octet)))
    (incf (%ike-message-id c))
    (send (%vpn-connection c) (%octets marker msg))))

(defmethod disconnect ((c ipsec-client))
  (dolist (exposure (copy-list (%exposures c)))
    (unexpose exposure))
  ;; tell the server to drop the SA (frees its pool lease); give the writer a
  ;; moment to flush before we tear the transport down.
  (ignore-errors (%ike-delete c) (sleep 0.2))
  (ignore-errors (bt:destroy-thread (%keepalive-thread c)))
  (disconnect (%vpn-connection c)))

(defun %nat-keepalive-loop (c)
  "Keep the NAT mapping for the ESP/IKE port alive (RFC 3948 §4): a lone
0xFF byte every 20s. Harmless if no NAT is present."
  (lambda ()
    (loop
      (sleep 20)
      (ignore-errors (send (%vpn-connection c) (make-array 1 :element-type 'octet
                                                             :initial-element #xff))))))

;;; --- receive path: demux IKE vs ESP, decrypt, dispatch ---------------------

(defun %ipsec-reader-callback (c)
  (lambda (buffer size)
    (let ((packet (subseq buffer 0 size)))
      (cond
        ;; a NAT-keepalive (single 0xFF) or anything too small: ignore.
        ((< size 8) nil)
        ;; non-ESP marker => an IKE message (DPD / INFORMATIONAL). We don't
        ;; initiate rekeys and disabled DPD server-side, so just ignore.
        ((and (>= size +non-esp-marker+)
              (every #'zerop (subseq packet 0 +non-esp-marker+)))
         nil)
        ;; otherwise an ESP packet: decrypt and dispatch the inner IP packet.
        (t
         (multiple-value-bind (inner next-header) (%esp-decrypt c packet)
           (when (and inner (= next-header 4))     ; 4 = inner IPv4
             (%dispatch-inner-packet c inner))))))))

(defun %dispatch-inner-packet (c inner-ip-bytes)
  "Parse a decrypted inner IPv4 packet far enough to hand it to the shared
demux (which keys connections by the 4-tuple)."
  (fs:with-input-from-sequence (s inner-ip-bytes)
    (let ((first-byte (fs:peek-byte s)))
      (when (= (ash first-byte -4) 4)               ; IPv4 version nibble
        (%dispatch-inner-ip c :ip (bin:read-binary 'ipv4-header s) s)))))

;;; --- send path --------------------------------------------------------------

(defmethod send-packet ((c ipsec-client) protocol key packet &key skip-connection)
  (let ((inner (coerce (fs:with-output-to-sequence (s) (bin:write-binary packet s))
                       'octet-vector)))
    (when (and (not skip-connection) protocol)
      (bt:with-lock-held ((%connections-lock c))
        (%ensure-connection-queue c protocol key)))
    (send (%vpn-connection c) (%esp-encrypt c inner))
    ;; ICMP is request/response: block for the reply, like the OpenVPN client.
    (when (eq protocol +icmp-protocol+)
      (receive-packet c protocol key))))

(defmethod ping ((c ipsec-client) dst-address)
  (let* ((dst-ip (string-ipv4-address-to-integer dst-address))
         (key (random #xffff))
         (packet (%make-ipv4-icmp-packet (%client-ip-address c) dst-ip key)))
    (send-packet c +icmp-protocol+ key packet)))
