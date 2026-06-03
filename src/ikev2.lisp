(in-package #:erebus)

;;; ---------------------------------------------------------------------------
;;; IKEv2 control plane (RFC 7296) + crypto, PSK authentication.
;;;
;;; This file is deliberately transport-agnostic: it builds and parses IKEv2
;;; messages as plain octet vectors and computes the key schedule, but never
;;; touches a socket. The synchronous handshake driver that ties these pieces
;;; to a UDP socket lives in esp.lisp.
;;;
;;; The negotiated suite is fixed (see the user-approved plan):
;;;   DH MODP-2048 (group 14), ENCR_AES_CBC/256, PRF_HMAC_SHA2_256,
;;;   AUTH_HMAC_SHA2_256_128, and for the CHILD_SA AES-256-CBC + SHA2-256-128.
;;; ---------------------------------------------------------------------------

;;; --- little-helpers: big-endian integer <-> octets -------------------------

(defun int->bytes (n size)
  "N as a SIZE-byte big-endian octet vector (left zero-padded)."
  (let ((v (make-array size :element-type 'octet)))
    (dotimes (i size v)
      (setf (aref v (- size 1 i)) (ldb (byte 8 (* 8 i)) n)))))

(defun bytes->int (bytes &optional (start 0) (end (length bytes)))
  "Decode BYTES[START,END) as a big-endian unsigned integer."
  (let ((n 0))
    (loop for i from start below end
          do (setf n (+ (ash n 8) (aref bytes i))))
    n))

(defun %octets (&rest parts)
  "Concatenate PARTS (octet vectors / lists / numbers) into one octet vector."
  (apply #'concatenate 'octet-vector
         (mapcar (lambda (p)
                   (etypecase p
                     (integer (vector p))
                     (sequence p)))
                 parts)))

;;; --- IKEv2 numeric constants -----------------------------------------------

;; payload types (also used as "next payload" links)
(defconstant +pl-none+   0)
(defconstant +pl-sa+     33)
(defconstant +pl-ke+     34)
(defconstant +pl-idi+    35)
(defconstant +pl-idr+    36)
(defconstant +pl-auth+   39)
(defconstant +pl-nonce+  40)
(defconstant +pl-notify+ 41)
(defconstant +pl-tsi+    44)
(defconstant +pl-tsr+    45)
(defconstant +pl-sk+     46)
(defconstant +pl-cp+     47)

;; exchange types
(defconstant +ex-sa-init+ 34)
(defconstant +ex-auth+    35)
(defconstant +ex-informational+ 37)

;; header flags
(defconstant +flag-initiator+ #x08)
(defconstant +flag-response+  #x20)

;; transform types
(defconstant +tt-encr+  1)
(defconstant +tt-prf+   2)
(defconstant +tt-integ+ 3)
(defconstant +tt-dh+    4)
(defconstant +tt-esn+   5)

;; transform ids for our fixed suite
(defconstant +encr-aes-cbc+ 12)
(defconstant +prf-hmac-sha2-256+ 5)
(defconstant +auth-hmac-sha2-256-128+ 12)
(defconstant +dh-group-14+ 14)
(defconstant +esn-none+ 0)

;; protocol ids
(defconstant +proto-ike+ 1)
(defconstant +proto-esp+ 3)

;; notify message types
(defconstant +nat-detection-source-ip+ 16388)
(defconstant +nat-detection-destination-ip+ 16389)

;; identification payload types
(defconstant +id-ipv4-addr+ 1)
(defconstant +id-fqdn+ 2)

;; configuration payload
(defconstant +cfg-request+ 1)
(defconstant +cfg-reply+ 2)
(defconstant +cfg-attr-internal-ip4-address+ 1)

;;; --- Diffie-Hellman, MODP group 14 (RFC 3526) ------------------------------

(defparameter +dh14-prime+
  (parse-integer
   (concatenate
    'string
    "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1"
    "29024E088A67CC74020BBEA63B139B22514A08798E3404DD"
    "EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245"
    "E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED"
    "EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D"
    "C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F"
    "83655D23DCA3AD961C62F356208552BB9ED529077096966D"
    "670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B"
    "E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9"
    "DE2BCBF6955817183995497CEA956AE515D2261898FA0510"
    "15728E5A8AACAA68FFFFFFFFFFFFFFFF")
   :radix 16)
  "The 2048-bit MODP prime of Diffie-Hellman group 14.")

(defconstant +dh14-generator+ 2)
(defconstant +dh14-bytes+ 256 "Group-14 public values are 2048 bits = 256 bytes.")

(defun make-dh-keypair ()
  "Return (values private-exponent public-bytes). A 256-bit private exponent
is ample for group 14 and keeps the modular exponentiation cheap."
  (let* ((x (bytes->int (ic:random-data 32)))
         (pub (ic:expt-mod +dh14-generator+ x +dh14-prime+)))
    (values x (int->bytes pub +dh14-bytes+))))

(defun dh-shared-secret (private peer-public-bytes)
  "The shared secret g^(xy) mod p, as a fixed 256-byte big-endian vector."
  (int->bytes (ic:expt-mod (bytes->int peer-public-bytes) private +dh14-prime+)
              +dh14-bytes+))

;;; --- PRF / prf+ / key schedule (RFC 7296 §2.13-2.17) -----------------------

(defun %prf (key data)
  "PRF_HMAC_SHA2_256."
  (let ((h (ic:make-hmac (coerce key 'octet-vector) :sha256)))
    (ic:update-hmac h (coerce data 'octet-vector))
    (ic:hmac-digest h)))

(defun %prf+ (key seed length)
  "RFC 7296 §2.13 prf+: T1=prf(K,S|1), Tn=prf(K,Tn-1|S|n), truncated to LENGTH."
  (let ((out (make-array 0 :element-type 'octet))
        (prev (make-array 0 :element-type 'octet)))
    (loop for i from 1
          while (< (length out) length)
          do (setf prev (%prf key (%octets prev seed i)))
             (setf out (%octets out prev)))
    (subseq out 0 length)))

(defun derive-ike-keys (ni nr shared spii spir)
  "Derive the seven IKE SA keys. NI/NR are the nonce *data*, SHARED is the DH
secret bytes, SPII/SPIR the two 8-byte IKE SPIs. Returns a plist."
  (let* ((skeyseed (%prf (%octets ni nr) shared))
         (km (%prf+ skeyseed (%octets ni nr spii spir) 224)))
    (list :sk-d  (subseq km 0 32)
          :sk-ai (subseq km 32 64)
          :sk-ar (subseq km 64 96)
          :sk-ei (subseq km 96 128)
          :sk-er (subseq km 128 160)
          :sk-pi (subseq km 160 192)
          :sk-pr (subseq km 192 224))))

(defun derive-esp-keys (sk-d ni nr)
  "CHILD_SA keying material, no PFS: KEYMAT = prf+(SK_d, Ni | Nr). Sliced, in
RFC order, into initiator-send then responder-send (encrypt before integrity).
We are the initiator, so :encr-out/:integ-out protect our outbound ESP and
:encr-in/:integ-in verify+decrypt inbound ESP."
  (let ((km (%prf+ sk-d (%octets ni nr) 128)))
    (list :encr-out  (subseq km 0 32)
          :integ-out (subseq km 32 64)
          :encr-in   (subseq km 64 96)
          :integ-in  (subseq km 96 128))))

;;; --- PSK authentication (RFC 7296 §2.15) -----------------------------------

(defun %psk-key (psk-bytes)
  (%prf psk-bytes (b:string-to-octets "Key Pad for IKEv2")))

(defun compute-psk-auth (psk-bytes signed-octets)
  "AUTH = prf(prf(PSK, \"Key Pad for IKEv2\"), SignedOctets)."
  (%prf (%psk-key psk-bytes) signed-octets))

(defun initiator-signed-octets (real-message-1 nr-data sk-pi idi-body)
  "RealMessage1 | Nr | prf(SK_pi, IDi'), where IDi' is the IDi payload body
\(everything after its 4-byte generic header)."
  (%octets real-message-1 nr-data (%prf sk-pi idi-body)))

(defun responder-signed-octets (real-message-2 ni-data sk-pr idr-body)
  "RealMessage2 | Ni | prf(SK_pr, IDr')."
  (%octets real-message-2 ni-data (%prf sk-pr idr-body)))

;;; --- payload-chain assembly / parsing --------------------------------------

(defun %chain-payloads (payloads)
  "PAYLOADS is a list of (type . body-octets). Prefix each body with its
4-byte generic payload header, linking next-payload fields. Returns
\(values chained-octets first-type)."
  (let ((vec (coerce payloads 'vector)))
    (values
     (apply #'concatenate 'octet-vector
            (loop for i below (length vec)
                  for (type . body) = (aref vec i)
                  for next = (if (< (1+ i) (length vec))
                                 (car (aref vec (1+ i)))
                                 +pl-none+)
                  collect (%octets next 0 (int->bytes (+ 4 (length body)) 2) body)))
     (if (plusp (length vec)) (car (aref vec 0)) +pl-none+))))

(defun parse-payloads (first-type bytes)
  "Walk a generic-payload chain. Returns a list of (type . body-octets) in
order, where BODY excludes the 4-byte generic header."
  (let ((result '()) (type first-type) (off 0) (len (length bytes)))
    (loop while (and (/= type +pl-none+) (< (+ off 4) (1+ len)))
          do (let* ((next (aref bytes off))
                    (plen (bytes->int bytes (+ off 2) (+ off 4)))
                    (body (subseq bytes (+ off 4) (+ off plen))))
               (push (cons type body) result)
               (setf type next)
               (incf off plen)))
    (nreverse result)))

;;; --- IKE header -------------------------------------------------------------

(defun build-ike-header (spii spir first-payload exchange flags message-id total-length)
  (%octets spii spir
           first-payload #x20 exchange flags        ; version major.minor = 2.0
           (int->bytes message-id 4)
           (int->bytes total-length 4)))

(defstruct ike-header spii spir next-payload exchange flags message-id length)

(defun parse-ike-header (bytes)
  (make-ike-header :spii (subseq bytes 0 8)
                   :spir (subseq bytes 8 16)
                   :next-payload (aref bytes 16)
                   :exchange (aref bytes 18)
                   :flags (aref bytes 19)
                   :message-id (bytes->int bytes 20 24)
                   :length (bytes->int bytes 24 28)))

(defun build-ike-message (spii spir exchange flags message-id payloads)
  "Assemble a cleartext IKE message (used for IKE_SA_INIT)."
  (multiple-value-bind (body first-type) (%chain-payloads payloads)
    (%octets (build-ike-header spii spir first-type exchange flags message-id
                               (+ 28 (length body)))
             body)))

;;; --- individual payload bodies ---------------------------------------------

(defun %transform (last-p type id &optional key-length)
  (let ((attrs (if key-length
                   (%octets (int->bytes #x800E 2) (int->bytes key-length 2)) ; AF=1,type=14
                   #())))
    (%octets (if last-p 0 3) 0
             (int->bytes (+ 8 (length attrs)) 2)
             type 0
             (int->bytes id 2)
             attrs)))

(defun %proposal (proposal-num protocol-id spi transforms)
  (let ((tbody (apply #'concatenate 'octet-vector transforms)))
    (%octets 0 0                                          ; last proposal, reserved
             (int->bytes (+ 8 (length spi) (length tbody)) 2)
             proposal-num protocol-id (length spi) (length transforms)
             spi tbody)))

(defun build-sa-ike ()
  "SA body proposing our single IKE suite."
  (%proposal 1 +proto-ike+ #()
             (list (%transform nil +tt-encr+  +encr-aes-cbc+ 256)
                   (%transform nil +tt-prf+   +prf-hmac-sha2-256+)
                   (%transform nil +tt-integ+ +auth-hmac-sha2-256-128+)
                   (%transform t   +tt-dh+    +dh-group-14+))))

(defun build-sa-esp (esp-spi)
  "SA body proposing our CHILD_SA (ESP) suite, offering ESP-SPI (4 bytes)."
  (%proposal 1 +proto-esp+ esp-spi
             (list (%transform nil +tt-encr+  +encr-aes-cbc+ 256)
                   (%transform nil +tt-integ+ +auth-hmac-sha2-256-128+)
                   (%transform t   +tt-esn+   +esn-none+))))

(defun build-ke (public-bytes)
  (%octets (int->bytes +dh-group-14+ 2) (int->bytes 0 2) public-bytes))

(defun build-nonce (nonce-bytes) (coerce nonce-bytes 'octet-vector))

(defun build-notify (protocol message-type spi data)
  (%octets protocol (length spi) (int->bytes message-type 2) spi data))

(defun nat-detection-hash (spii spir ip port)
  "SHA1(SPIi | SPIr | IP | Port) -- the NAT-detection notification data."
  (ic:digest-sequence :sha1 (%octets spii spir (int->bytes ip 4) (int->bytes port 2))))

(defun build-idi (id-type id-data)
  (%octets id-type 0 0 0 id-data))

(defun build-auth (auth-data)
  (%octets 2 0 0 0 auth-data))                          ; method 2 = shared-key MIC

(defun build-ts-full ()
  "A single traffic selector matching every IPv4 address/port/protocol."
  (%octets 1 0 0 0                                       ; one TS, reserved
           7 0                                           ; TS_IPV4_ADDR_RANGE, proto any
           (int->bytes 16 2)                             ; selector length
           (int->bytes 0 2) (int->bytes 65535 2)         ; port range
           (int->bytes 0 4) (int->bytes #xffffffff 4)))  ; 0.0.0.0 .. 255.255.255.255

(defun build-cp-request-ip4 ()
  "CFG_REQUEST asking the responder to assign us an INTERNAL_IP4_ADDRESS."
  (%octets +cfg-request+ 0 0 0
           (int->bytes +cfg-attr-internal-ip4-address+ 2) (int->bytes 0 2)))

(defconstant +pl-delete+ 42)

(defun build-delete-ike ()
  "A Delete payload for the whole IKE SA: protocol IKE, no SPIs (deleting the
IKE SA implicitly deletes its CHILD_SAs). Sent in an INFORMATIONAL exchange so
the responder tears the SA down and frees our pool lease."
  (%octets +proto-ike+ 0 (int->bytes 0 2)))

;;; --- encrypted (SK) payload -------------------------------------------------

(defun build-encrypted-ike-message (spii spir exchange flags message-id
                                     inner-payloads sk-e sk-a)
  "Build an IKE message whose payloads are wrapped in an Encrypted (SK)
payload: AES-256-CBC under SK-E with a fresh IV, then HMAC-SHA2-256-128 under
SK-A over the whole message (encrypt-then-MAC)."
  (multiple-value-bind (plaintext first-inner-type) (%chain-payloads inner-payloads)
    (let* ((iv (ic:random-data 16))
           (padlen (mod (- 16 (mod (1+ (length plaintext)) 16)) 16))
           (to-encrypt (%octets plaintext
                                (make-array padlen :element-type 'octet)
                                padlen))
           (cipher (ic:make-cipher :aes :mode :cbc :key sk-e :initialization-vector iv))
           (ct (make-array (length to-encrypt) :element-type 'octet)))
      (ic:encrypt cipher to-encrypt ct)
      (let* ((sk-body (%octets iv ct))                   ; everything but the ICV
             (sk-len (+ 4 (length sk-body) 16))          ; +header +ICV
             (header (build-ike-header spii spir +pl-sk+ exchange flags message-id
                                       (+ 28 sk-len)))
             (sk-header (%octets first-inner-type 0 (int->bytes sk-len 2)))
             (mac-data (%octets header sk-header sk-body))
             (icv (subseq (%prf sk-a mac-data) 0 16)))
        (%octets mac-data icv)))))

(defun parse-encrypted-ike-message (bytes sk-e sk-a)
  "Verify the ICV (constant-time) and decrypt an SK-wrapped IKE message.
Returns (values ike-header inner-payloads-alist). Signals on a bad ICV."
  (let* ((header (parse-ike-header bytes))
         (sk-off 28)
         (sk-next (aref bytes sk-off))
         (sk-len (bytes->int bytes (+ sk-off 2) (+ sk-off 4)))
         (sk-end (+ sk-off sk-len))
         (icv (subseq bytes (- sk-end 16) sk-end))
         (mac-data (subseq bytes 0 (- sk-end 16)))
         (expected (subseq (%prf sk-a mac-data) 0 16)))
    (assert (ic:constant-time-equal icv expected) () "IKE SK integrity check failed")
    (let* ((iv (subseq bytes (+ sk-off 4) (+ sk-off 20)))
           (ct (subseq bytes (+ sk-off 20) (- sk-end 16)))
           (cipher (ic:make-cipher :aes :mode :cbc :key sk-e :initialization-vector iv))
           (pt (make-array (length ct) :element-type 'octet)))
      (ic:decrypt cipher ct pt)
      (let* ((padlen (aref pt (1- (length pt))))
             (plaintext (subseq pt 0 (- (length pt) padlen 1))))
        (values header (parse-payloads sk-next plaintext))))))

;;; --- response-parsing helpers ----------------------------------------------

(defun payload-body (type alist)
  "First payload body of TYPE in an alist from PARSE-PAYLOADS, or NIL."
  (cdr (assoc type alist)))

(defun sa-first-spi (sa-body)
  "The SPI carried by the (single) proposal in an SA payload body, or NIL."
  (let ((spi-size (aref sa-body 6)))
    (when (plusp spi-size)
      (subseq sa-body 8 (+ 8 spi-size)))))

(defun notify-message-type (notify-body)
  (bytes->int notify-body 2 4))

(defun cp-internal-ip4 (cp-body)
  "The INTERNAL_IP4_ADDRESS (as a 32-bit integer) assigned in a CFG_REPLY, or
NIL if absent."
  (let ((off 4) (len (length cp-body)))
    (loop while (<= (+ off 4) len)
          do (let* ((atype (logand (bytes->int cp-body off (+ off 2)) #x7fff))
                    (alen (bytes->int cp-body (+ off 2) (+ off 4))))
               (when (and (= atype +cfg-attr-internal-ip4-address+) (>= alen 4))
                 (return-from cp-internal-ip4 (bytes->int cp-body (+ off 4) (+ off 8))))
               (incf off (+ 4 alen))))
    nil))

(defun error-notify-type (alist)
  "If the message carries an IKEv2 error notify (type < 16384), return that
type, else NIL."
  (loop for (type . body) in alist
        when (= type +pl-notify+)
          do (let ((mt (notify-message-type body)))
               (when (< mt 16384) (return mt)))))
