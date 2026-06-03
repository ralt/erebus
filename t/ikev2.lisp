(in-package #:erebus/test)

;;; Pure, fast unit tests for the IKEv2 codecs and key schedule (no docker).
;;; The live strongSwan interop tests live in t/ipsec.lisp.

(def-suite* erebus/ikev2 :in erebus)

(defmacro with-ike (() &body body)
  "Run BODY in the EREBUS package so the internal IKEv2 symbols are reachable
without a forest of EREBUS:: prefixes."
  `(let ((*package* (find-package :erebus))) ,@body))

(test int-bytes-roundtrip
  (is (= 305419896 (erebus::bytes->int (erebus::int->bytes 305419896 4))))
  (is (equalp #(0 1) (erebus::int->bytes 1 2)))
  (is (= 4 (length (erebus::int->bytes #xdeadbeef 4))))
  ;; left zero-padding when the value is smaller than the field
  (is (equalp #(0 0 0 5) (erebus::int->bytes 5 4))))

(test dh-group-14-agreement
  ;; both sides must compute the same shared secret, and public values are
  ;; the fixed group-14 width.
  (multiple-value-bind (xa pa) (erebus::make-dh-keypair)
    (multiple-value-bind (xb pb) (erebus::make-dh-keypair)
      (is (= 256 (length pa)))
      (is (= 256 (length pb)))
      (is (equalp (erebus::dh-shared-secret xa pb)
                  (erebus::dh-shared-secret xb pa))))))

(test prf+-shape
  (let ((k (ironclad:random-data 32))
        (s (ironclad:random-data 40)))
    (is (= 224 (length (erebus::%prf+ k s 224))))
    ;; deterministic for the same inputs
    (is (equalp (erebus::%prf+ k s 100) (erebus::%prf+ k s 100)))
    ;; the first block is exactly prf(k, s|1)
    (is (equalp (subseq (erebus::%prf+ k s 32) 0 32)
                (erebus::%prf k (erebus::%octets s 1))))))

(test ike-key-schedule-shape
  (let ((ks (erebus::derive-ike-keys (ironclad:random-data 32) (ironclad:random-data 32)
                                     (ironclad:random-data 256)
                                     (ironclad:random-data 8) (ironclad:random-data 8))))
    (dolist (key '(:sk-d :sk-ai :sk-ar :sk-ei :sk-er :sk-pi :sk-pr))
      (is (= 32 (length (getf ks key))))))
  (let ((esp (erebus::derive-esp-keys (ironclad:random-data 32)
                                      (ironclad:random-data 32) (ironclad:random-data 32))))
    (dolist (key '(:encr-out :integ-out :encr-in :integ-in))
      (is (= 32 (length (getf esp key)))))))

(test cleartext-message-roundtrip
  (with-ike ()
    (multiple-value-bind (x pub) (erebus::make-dh-keypair)
      (declare (ignore x))
      (let* ((spii (ironclad:random-data 8))
             (spir (make-array 8 :element-type '(unsigned-byte 8)))
             (ni (ironclad:random-data 32))
             (msg (erebus::build-ike-message
                   spii spir erebus::+ex-sa-init+ erebus::+flag-initiator+ 0
                   (list (cons erebus::+pl-sa+ (erebus::build-sa-ike))
                         (cons erebus::+pl-ke+ (erebus::build-ke pub))
                         (cons erebus::+pl-nonce+ (erebus::build-nonce ni)))))
             (hdr (erebus::parse-ike-header msg))
             (pls (erebus::parse-payloads (erebus::ike-header-next-payload hdr)
                                          (subseq msg 28))))
        (is (equalp spii (erebus::ike-header-spii hdr)))
        (is (= erebus::+ex-sa-init+ (erebus::ike-header-exchange hdr)))
        (is (= (length msg) (erebus::ike-header-length hdr)))
        (is (= 3 (length pls)))
        (is (equalp ni (erebus::payload-body erebus::+pl-nonce+ pls)))
        (is (= erebus::+dh-group-14+
               (erebus::bytes->int (erebus::payload-body erebus::+pl-ke+ pls) 0 2)))))))

(test encrypted-message-roundtrip
  (let* ((spii (ironclad:random-data 8))
         (spir (ironclad:random-data 8))
         (sk-e (ironclad:random-data 32))
         (sk-a (ironclad:random-data 32))
         (idi (erebus::build-idi erebus::+id-fqdn+ (babel:string-to-octets "erebus")))
         (auth (erebus::build-auth (ironclad:random-data 32)))
         (msg (erebus::build-encrypted-ike-message
               spii spir erebus::+ex-auth+ erebus::+flag-initiator+ 1
               (list (cons erebus::+pl-idi+ idi)
                     (cons erebus::+pl-auth+ auth))
               sk-e sk-a)))
    (multiple-value-bind (hdr pls) (erebus::parse-encrypted-ike-message msg sk-e sk-a)
      (is (= erebus::+ex-auth+ (erebus::ike-header-exchange hdr)))
      (is (= 2 (length pls)))
      (is (equalp idi (erebus::payload-body erebus::+pl-idi+ pls)))
      (is (equalp auth (erebus::payload-body erebus::+pl-auth+ pls))))))

(test encrypted-message-tamper-rejected
  (let* ((sk-e (ironclad:random-data 32))
         (sk-a (ironclad:random-data 32))
         (msg (erebus::build-encrypted-ike-message
               (ironclad:random-data 8) (ironclad:random-data 8)
               erebus::+ex-auth+ erebus::+flag-initiator+ 1
               (list (cons erebus::+pl-idi+ (erebus::build-idi erebus::+id-fqdn+ #(1 2 3))))
               sk-e sk-a)))
    ;; flip a bit in the ICV
    (setf (aref msg (1- (length msg))) (logxor 1 (aref msg (1- (length msg)))))
    (signals error (erebus::parse-encrypted-ike-message msg sk-e sk-a))))

(test psk-auth-deterministic
  ;; the PSK AUTH value is a pure function of its inputs
  (let* ((psk (babel:string-to-octets "secret"))
         (octets (ironclad:random-data 128)))
    (is (equalp (erebus::compute-psk-auth psk octets)
                (erebus::compute-psk-auth psk octets)))
    (is (= 32 (length (erebus::compute-psk-auth psk octets))))))
