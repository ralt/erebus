(in-package #:erebus)

(bin:defbinary ipv4-header (:byte-order :big-endian)
  (version 4 :type (unsigned-byte 4))
  (ihl 5 :type (unsigned-byte 4))
  (tos 0 :type (unsigned-byte 8))
  (total-length 0 :type (unsigned-byte 16))
  (identification 0 :type (unsigned-byte 16))
  (flags 2 :type (unsigned-byte 3))
  (fragment-offset 0 :type (unsigned-byte 13))
  (ttl 64 :type (unsigned-byte 8))
  (protocol 0 :type (unsigned-byte 8))
  (header-checksum 0 :type (unsigned-byte 16))
  (src-ip 0 :type (unsigned-byte 32))
  (dst-ip 0 :type (unsigned-byte 32)))

(defconstant +icmp-echo-request+ 8)

(bin:defbinary icmp-packet (:byte-order :big-endian)
  (type +icmp-echo-request+ :type (unsigned-byte 8))
  (code 0 :type (unsigned-byte 8))
  (checksum 0 :type (unsigned-byte 16))
  (identifier 0 :type (unsigned-byte 16))
  (sequence-number 0 :type (unsigned-byte 16)))

(defun packet-bytes-checksum (packet)
  (let ((packet-bytes (fs:with-output-to-sequence (s)
                        (bin:write-binary packet s))))
    (values packet-bytes (%sum-16bits packet-bytes))))

(defun %sum-16bits (bytes &optional (sum 0))
  (let* ((hi (elt bytes 0))
         (lo (elt bytes 1))
         (new-sum (+ sum (ash hi 8) lo)))
    (if (= (length bytes) 2)
        (logand (lognot (%wrap-around-carry new-sum)) #xffff)
        (%sum-16bits (subseq bytes 2) new-sum))))

(defun %wrap-around-carry (sum)
  (if (< sum #xffff)
      sum
      (%wrap-around-carry (+ (logand sum #xffff) (ash sum -16)))))

(defconstant +icmp-protocol+ 1)

(bin:defbinary ipv4-icmp-packet (:byte-order :big-endian)
  (ipv4-header nil :type ipv4-header)
  (icmp-packet nil :type icmp-packet))

(defun %make-ipv4-icmp-packet (src-ip dst-ip &optional (identifier 0) (sequence-number 0))
  (let ((icmp-packet (make-icmp-packet :identifier identifier :sequence-number sequence-number)))
    (multiple-value-bind (icmp-packet-bytes icmp-checksum)
        (packet-bytes-checksum icmp-packet)
      (setf (icmp-packet-checksum icmp-packet) icmp-checksum)

      (let ((ipv4-header (make-ipv4-header
                          :total-length (+ 20 (length icmp-packet-bytes))
                          :protocol +icmp-protocol+
                          :src-ip src-ip
                          :dst-ip dst-ip)))
        (multiple-value-bind (ipv4-header-bytes ipv4-header-checksum)
            (packet-bytes-checksum ipv4-header)
          (declare (ignore ipv4-header-bytes))
          (setf (ipv4-header-header-checksum ipv4-header)
                ipv4-header-checksum)

          (make-ipv4-icmp-packet :ipv4-header ipv4-header
                                 :icmp-packet icmp-packet))))))

(defun string-ipv4-address-to-integer (string-address)
  (let* ((parts (uiop:split-string string-address :separator "."))
         (int-parts (mapcar (lambda (part) (parse-integer part)) parts)))
    (+ (ash (elt int-parts 0) 24)
       (ash (elt int-parts 1) 16)
       (ash (elt int-parts 2) 8)
       (elt int-parts 3))))
