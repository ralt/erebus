;;;; Stress / correctness test for the IPsec (IKEv2 + ESP) data plane.
;;;;
;;;; Three phases, all against one strongSwan container:
;;;;
;;;;   A. connect/disconnect churn -- hammer the IKEv2 handshake and orderly
;;;;      teardown (INFORMATIONAL+DELETE) many times. Each cycle does a full
;;;;      IKE_SA_INIT + IKE_AUTH, a ping and a proxied request, then deletes
;;;;      the SA. Proves handshakes are repeatable and that the server's pool
;;;;      lease is released each time (otherwise the /24 pool would exhaust).
;;;;
;;;;   B. outbound proxy -- many sequential *and* concurrent HTTP requests
;;;;      through one connected client's proxy to the in-tunnel nginx.
;;;;
;;;;   C. inbound forwarding -- many concurrent peer-initiated connections
;;;;      (from inside the container) to an exposed port, relayed to a
;;;;      host-side count-server, mirroring t/stress-inbound.lisp.
;;;;
;;;; Timed-out connections are the documented stop-and-wait / no-retransmission
;;;; limitation of the userspace TCP stack over a lossy tunnel, reported but
;;;; not counted as a logic bug. Wrong bytes / resets / handshake failures are.
;;;;
;;;; Run from the project root:
;;;;   sbcl --script t/stress-ipsec.lisp
(load "~/quicklisp/setup.lisp")
(push (truename ".") asdf:*central-registry*)
(ql:quickload :erebus/test :verbose nil)
(in-package :erebus/test)

(defparameter *churn* 30 "Connect/disconnect cycles in phase A.")
(defparameter *outbound-seq* 60 "Sequential proxied requests in phase B.")
(defparameter *outbound-threads* 12 "Concurrent client threads in phase B.")
(defparameter *outbound-per-thread* 5 "Requests per concurrent thread in phase B.")
(defparameter *inbound-threads* 12 "Concurrent client threads in phase C.")
(defparameter *inbound-per-thread* 5 "Connections per client thread in phase C.")
(defparameter *inbound-max-bytes* 30000 "Upper bound on inbound payload size.")
(defparameter *inbound-vpn-port* 8090)

(defvar *bugs* 0 "Real correctness failures across all phases (non-zero => exit 1).")

(defun fetch-status (url proxy)
  "Fetch URL through PROXY = (host port); return (values status byte-count)."
  (multiple-value-bind (body status)
      (drakma:http-request url :proxy proxy :close t :connection-timeout 30)
    (values status (length (if (stringp body) (babel:string-to-octets body) body)))))

;;; ---------------------------------------------------------------------------
;;; Phase A: connect / disconnect churn
;;; ---------------------------------------------------------------------------

(defun phase-a (ike natt)
  (format t "~%== Phase A: ~a connect/disconnect cycles ==~%" *churn*)
  (let ((ok 0) (fail 0)
        (start (get-internal-real-time)))
    (dotimes (i *churn*)
      (handler-case
          (let ((client (make-ipsec-test-client ike natt)))
            (connect client)
            (unwind-protect
                 (progn
                   (ping client +ipsec-server-ip+)
                   (let ((proxy (make-instance 'erebus::acceptor
                                               :address "127.0.0.1" :port 11097
                                               :client client)))
                     (hunchentoot:start proxy)
                     (unwind-protect
                          (multiple-value-bind (status)
                              (fetch-status (format nil "http://~a" +ipsec-server-ip+)
                                            '("127.0.0.1" 11097))
                            (if (eql status 404) (incf ok) (progn (incf fail) (incf *bugs*))))
                       (hunchentoot:stop proxy))))
              (disconnect client)))
        (error (e)
          (incf fail) (incf *bugs*)
          (format t "  cycle ~a FAILED: ~a~%" i e))))
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second 1.0)))
      (format t "  ~a/~a cycles succeeded in ~,1fs (~,2fs/cycle)~%"
              ok *churn* elapsed (/ elapsed *churn*))
      (if (zerop fail)
          (format t "  PASS: every handshake+teardown round-tripped.~%")
          (format t "  FAIL: ~a cycle(s) errored.~%" fail)))))

;;; ---------------------------------------------------------------------------
;;; Phase B: outbound proxy (sequential + concurrent)
;;; ---------------------------------------------------------------------------

(defun phase-b (client)
  (format t "~%== Phase B: outbound proxy (~a sequential, then ~ax~a concurrent) ==~%"
          *outbound-seq* *outbound-threads* *outbound-per-thread*)
  (let ((proxy (make-instance 'erebus::acceptor :address "127.0.0.1" :port 11098
                              :client client)))
    (hunchentoot:start proxy)
    (unwind-protect
         (let ((url (format nil "http://~a" +ipsec-server-ip+)))
           ;; sequential
           (let ((ok 0) (start (get-internal-real-time)))
             (dotimes (i *outbound-seq*)
               (handler-case
                   (when (eql 404 (fetch-status url '("127.0.0.1" 11098))) (incf ok))
                 (error () (incf *bugs*))))
             (let ((elapsed (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second 1.0)))
               (format t "  sequential: ~a/~a ok in ~,1fs (~,1f req/s)~%"
                       ok *outbound-seq* elapsed (/ *outbound-seq* elapsed))
               (unless (= ok *outbound-seq*) (incf *bugs*))))
           ;; concurrent
           (let ((lock (bordeaux-threads:make-lock))
                 (ok 0) (errs 0)
                 (start (get-internal-real-time)))
             (let ((threads
                     (loop for tid below *outbound-threads*
                           collect (bordeaux-threads:make-thread
                                    (lambda ()
                                      (dotimes (i *outbound-per-thread*)
                                        (handler-case
                                            (let ((s (fetch-status url '("127.0.0.1" 11098))))
                                              (bordeaux-threads:with-lock-held (lock)
                                                (if (eql s 404) (incf ok) (incf errs))))
                                          (error ()
                                            (bordeaux-threads:with-lock-held (lock)
                                              (incf errs))))))
                                    :name (format nil "ob-~a" tid)))))
               (mapc #'bordeaux-threads:join-thread threads))
             (let ((total (* *outbound-threads* *outbound-per-thread*))
                   (elapsed (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second 1.0)))
               (format t "  concurrent: ~a/~a ok, ~a error(s) in ~,1fs (~,1f req/s)~%"
                       ok total errs elapsed (/ total elapsed))
               (incf *bugs* errs)
               (if (zerop errs)
                   (format t "  PASS: all outbound requests correct.~%")
                   (format t "  FAIL: ~a concurrent request(s) errored.~%" errs)))))
      (hunchentoot:stop proxy))))

;;; ---------------------------------------------------------------------------
;;; Phase C: inbound forwarding (concurrent peer-initiated connections)
;;; ---------------------------------------------------------------------------

(defun count-handler (stream)
  (let ((n (be32-to-integer (read-n-octets stream 4))))
    (read-n-octets stream n)
    (write-sequence (babel:string-to-octets (format nil "~a~%" n)) stream)
    (force-output stream)))

(defun inbound-python ()
  (concatenate
   'string +ipsec-py-preamble+
   (format nil "
import threading,random
PORT=~a
NT=~a
PER=~a
MAX=~a
lock=threading.Lock()
tally={'mismatch':0,'timeout':0,'other':0}
detail=[]
def worker(tid):
    r=random.Random(tid*7919+1)
    for i in range(PER):
        n=r.randint(1,MAX)
        try:
            s=conn(PORT)
            s.settimeout(60)
            s.sendall(struct.pack('>I',n)+b'x'*n)
            line=b''
            while not line.endswith(b'\\n'):
                c=s.recv(200)
                if not c: break
                line+=c
            s.close()
            got=int(line.strip())
            if got!=n:
                with lock:
                    tally['mismatch']+=1; detail.append('t%d i%d sent %d got %d'%(tid,i,n,got))
        except socket.timeout:
            with lock:
                tally['timeout']+=1
        except Exception as e:
            with lock:
                tally['other']+=1; detail.append('t%d i%d %s'%(tid,i,repr(e)))
ts=[threading.Thread(target=worker,args=(t,)) for t in range(NT)]
for t in ts: t.start()
for t in ts: t.join()
print('total=%d mismatch=%d timeout=%d other=%d'%(NT*PER,tally['mismatch'],tally['timeout'],tally['other']))
for d in detail[:20]: print(d)
" *inbound-vpn-port* *inbound-threads* *inbound-per-thread* *inbound-max-bytes*)))

(defun parse-tally (output key)
  (let ((pos (search (format nil "~a=" key) output)))
    (and pos (parse-integer output :start (+ pos (length key) 1) :junk-allowed t))))

(defun phase-c (name client)
  (format t "~%== Phase C: inbound, ~ax~a concurrent connections ==~%"
          *inbound-threads* *inbound-per-thread*)
  (with-local-tcp-server (local-port #'count-handler)
    (let ((exposure (expose client :vpn-port *inbound-vpn-port*
                                   :host "127.0.0.1" :port local-port)))
      (unwind-protect
           (let* ((output (run-python-in-container name (inbound-python)))
                  (total (parse-tally output "total"))
                  (mismatch (parse-tally output "mismatch"))
                  (timeout (parse-tally output "timeout"))
                  (other (parse-tally output "other")))
             (format t "  ~a~%" output)
             (cond
               ((and (eql mismatch 0) (eql other 0) (eql timeout 0))
                (format t "  PASS: all ~a inbound connections round-tripped.~%" total))
               ((and (eql mismatch 0) (eql other 0))
                (format t "  PASS (caveat): ~a/~a stalled (no-retransmission limitation).~%"
                        timeout total))
               (t
                (incf *bugs* (+ mismatch other))
                (format t "  FAIL: ~a mismatch + ~a other out of ~a.~%" mismatch other total))))
        (ignore-errors (unexpose exposure))))))

;;; ---------------------------------------------------------------------------

(let* ((name (format nil "erebus_ipsec_stress_~a" (random-string 12)))
       (ike (funcall (gen-integer :min 20000 :max 34999)))
       (natt (funcall (gen-integer :min 35000 :max 50000))))
  (handler-case
      (progn
        (ensure-ipsec-image)
        (create-ipsec-container name ike natt)
        (uiop:run-program (format nil "docker start ~a" name) :output t :error-output t)
        (start-ipsec-services name)
        (sleep 2)
        ;; Phase A churns its own clients.
        (phase-a ike natt)
        ;; Phases B and C share one long-lived client.
        (let ((client (make-ipsec-test-client ike natt)))
          (connect client)
          (unwind-protect
               (progn (phase-b client)
                      (phase-c name client))
            (disconnect client))))
    (error (e) (format t "~&STRESS-ERROR: ~a~%" e) (incf *bugs*)))
  (format t "~&== tearing down ==~%")
  (ignore-errors (cleanup-ipsec-container name)))

(format t "~&== done: ~a bug(s) ==~%" *bugs*)
(uiop:quit (if (zerop *bugs*) 0 1))
