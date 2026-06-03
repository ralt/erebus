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

(defconstant +max-32-bytes+ (expt 2 32))

(bin:defbinary icmp-packet (:byte-order :big-endian)
  (type +icmp-echo-request+ :type (unsigned-byte 8))
  (code 0 :type (unsigned-byte 8))
  (checksum 0 :type (unsigned-byte 16))
  (identifier 0 :type (unsigned-byte 16))
  (sequence-number 0 :type (unsigned-byte 16)))

(bin:defbinary tcp-header (:byte-order :big-endian)
  (src-port 0 :type (unsigned-byte 16))
  (dst-port 0 :type (unsigned-byte 16))
  (seqno 0 :type (unsigned-byte 32))
  (ackno 0 :type (unsigned-byte 32))
  (doffset 5 :type (unsigned-byte 4))
  (rsrvd 0 :type (unsigned-byte 4))

  (cwr 0 :type (unsigned-byte 1))
  (ece 0 :type (unsigned-byte 1))
  (urg 0 :type (unsigned-byte 1))
  (ack 0 :type (unsigned-byte 1))
  (psh 0 :type (unsigned-byte 1))
  (rst 0 :type (unsigned-byte 1))
  (syn 0 :type (unsigned-byte 1))
  (fin 0 :type (unsigned-byte 1))

  (window 0 :type (unsigned-byte 16))
  (checksum 0 :type (unsigned-byte 16))
  (urgent-pointer 0 :type (unsigned-byte 16)))

(bin:defbinary tcp-pseudo-header-for-checksum (:byte-order :big-endian)
  (src-address 0 :type (unsigned-byte 32))
  (dst-address 0 :type (unsigned-byte 32))
  (zeroes 0 :type (unsigned-byte 8))
  (protocol 6 :type (unsigned-byte 8))
  (total-length 0 :type (unsigned-byte 16)))

(defun packet-bytes-checksum (packets)
  (let ((packet-bytes (fs:with-output-to-sequence (s)
                        (dolist (packet packets)
                          (cond ((vectorp packet)
                                 (write-sequence packet s))
                                (t
                                 (bin:write-binary packet s)))))))
    (values packet-bytes (%sum-16bits packet-bytes))))

(defun %sum-16bits (bytes &optional (sum 0))
  (let* ((hi (elt bytes 0))
         (lo (elt bytes 1))
         (new-sum (+ sum (ash hi 8) lo)))
    (if (= (length bytes) 2)
        (logand (lognot (%wrap-around-carry new-sum)) #xffff)
        (%sum-16bits (let ((future-bytes (subseq bytes 2)))
                       (if (< (length future-bytes) 2)
                           (concatenate 'octet-vector future-bytes #(0))
                           future-bytes))
                     new-sum))))

(defun %wrap-around-carry (sum)
  (if (< sum #xffff)
      sum
      (%wrap-around-carry (+ (logand sum #xffff) (ash sum -16)))))

(defconstant +icmp-protocol+ 1)
(defconstant +tcp-protocol+ 6)

(bin:defbinary ipv4-icmp-packet (:byte-order :big-endian)
  (ipv4-header nil :type ipv4-header)
  (icmp-packet nil :type icmp-packet))

(defun %make-ipv4-icmp-packet (src-ip dst-ip &optional (identifier 0) (sequence-number 0))
  (let ((icmp-packet (make-icmp-packet :identifier identifier :sequence-number sequence-number)))
    (multiple-value-bind (icmp-packet-bytes icmp-checksum)
        (packet-bytes-checksum (list icmp-packet))
      (setf (icmp-packet-checksum icmp-packet) icmp-checksum)

      (let ((ipv4-header (make-ipv4-header
                          :total-length (+ 20 (length icmp-packet-bytes))
                          :protocol +icmp-protocol+
                          :src-ip src-ip
                          :dst-ip dst-ip)))
        (multiple-value-bind (ipv4-header-bytes ipv4-header-checksum)
            (packet-bytes-checksum (list ipv4-header))
          (declare (ignore ipv4-header-bytes))
          (setf (ipv4-header-header-checksum ipv4-header)
                ipv4-header-checksum)

          (make-ipv4-icmp-packet :ipv4-header ipv4-header
                                 :icmp-packet icmp-packet))))))

(bin:defbinary ipv4-tcp-packet (:byte-order :big-endian)
  (ipv4-header nil :type ipv4-header)
  (tcp-header nil :type tcp-header)
  (payload #() :type (simple-array (unsigned-byte 8) ((- (ipv4-header-total-length ipv4-header)
                                                         20
                                                         (length
                                                          (fs:with-output-to-sequence (s)
                                                            (bin:write-binary tcp-header s))))))))

(defun %make-ipv4-tcp-packet (src-ip src-port dst-ip dst-port &key
                                                                (seqno 0)
                                                                (ackno 0)
                                                                (syn 0)
                                                                (ack 0)
                                                                (psh 0)
                                                                (rst 0)
                                                                (fin 0)
                                                                (window 1024)
                                                                data)
  (let* ((tcp-header (make-tcp-header :src-port src-port
                                      :dst-port dst-port
                                      :seqno seqno
                                      :ackno ackno
                                      :syn syn
                                      :ack ack
                                      :psh psh
                                      :rst rst
                                      :fin fin
                                      :window window))
         (tcp-header-length (length
                             (fs:with-output-to-sequence (s)
                               (bin:write-binary tcp-header s))))
         (tcp-pseudo-header (make-tcp-pseudo-header-for-checksum
                             :src-address src-ip
                             :dst-address dst-ip
                             :total-length (+ tcp-header-length (length (or data #()))))))

    (multiple-value-bind (tcp-header-bytes checksum)
        (packet-bytes-checksum (list tcp-pseudo-header tcp-header (or data #())))
      (declare (ignore tcp-header-bytes))
      (setf (tcp-header-checksum tcp-header) checksum)

      (let ((ipv4-header (make-ipv4-header :total-length (+ 20
                                                            tcp-header-length
                                                            (length (or data #())))
                                           :protocol +tcp-protocol+
                                           :src-ip src-ip
                                           :dst-ip dst-ip)))
        (multiple-value-bind (ipv4-header-bytes ipv4-header-checksum)
            (packet-bytes-checksum (list ipv4-header))
          (declare (ignore ipv4-header-bytes))
          (setf (ipv4-header-header-checksum ipv4-header) ipv4-header-checksum)

          (make-ipv4-tcp-packet :ipv4-header ipv4-header
                                :tcp-header tcp-header
                                :payload (or data (make-array 0 :element-type 'octet))))))))

(defun string-ipv4-address-to-integer (string-address)
  (let* ((parts (uiop:split-string string-address :separator "."))
         (int-parts (mapcar (lambda (part) (parse-integer part)) parts)))
    (+ (ash (elt int-parts 0) 24)
       (ash (elt int-parts 1) 16)
       (ash (elt int-parts 2) 8)
       (elt int-parts 3))))
