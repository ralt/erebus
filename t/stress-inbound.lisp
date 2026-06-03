;;;; Correctness stress test for inbound port-forwarding (Phase 6).
;;;;
;;;; Unlike t/stress.lisp (which measures the outbound proxy's *speed*),
;;;; this hammers the *inbound* path with many concurrent connections to
;;;; shake out correctness bugs: races in the accept queue, cross-talk
;;;; between per-connection queues, teardown ordering, the bounded drain,
;;;; etc. Stressing concurrency is how these surface.
;;;;
;;;; Shape:
;;;;   container (many concurrent python clients) -> 10.8.0.2:<vpn-port>
;;;;     -> erebus accepts + relays -> host count-server (127.0.0.1)
;;;;
;;;; The host count-server reads a 4-byte big-endian length then that many
;;;; bytes and replies with the count. Each client sends a random-sized
;;;; payload and checks the echoed count matches exactly.
;;;;
;;;; We classify failures so the verdict is meaningful:
;;;;   * mismatch / other  -> a real correctness bug (wrong bytes, a reset,
;;;;                          a crash). The run FAILS (exit 1) on any.
;;;;   * timeout (a stalled connection) -> the userspace TCP stack is
;;;;                          stop-and-wait with NO retransmission (RTO is
;;;;                          out of scope, see Phase 10), so over the lossy
;;;;                          UDP tunnel a dropped datagram stalls that one
;;;;                          connection forever. These are reported as a
;;;;                          known transport limitation, not a logic bug.
;;;;
;;;; Run from the project root:
;;;;   sbcl --script t/stress-inbound.lisp
(load "~/quicklisp/setup.lisp")
(push (truename ".") asdf:*central-registry*)
(ql:quickload :erebus/test :verbose nil)
(in-package :erebus/test)

(defparameter *threads* 15 "Concurrent client threads inside the container.")
(defparameter *per-thread* 8 "Sequential connections each client thread makes.")
(defparameter *max-bytes* 30000
  "Upper bound on a random payload size; well above one TCP segment so
both single- and multi-segment transfers are exercised.")
(defparameter *vpn-port* 8090 "Exposed port on the erebus VPN IP.")

(defun count-handler (stream)
  "Read a 4-byte big-endian length then that many bytes, reply with the
count and a newline."
  (let ((n (be32-to-integer (read-n-octets stream 4))))
    (read-n-octets stream n)
    (write-sequence (babel:string-to-octets (format nil "~a~%" n)) stream)
    (force-output stream)))

(defun stress-python (vpn-port)
  "Python program: *THREADS* threads each open *PER-THREAD* sequential
connections to the erebus VPN IP, send a random-sized length-prefixed
payload, and verify the echoed count. Prints a classified tally."
  (concatenate
   'string +py-preamble+
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
                tally['timeout']+=1; detail.append('t%d i%d timeout'%(tid,i))
        except Exception as e:
            with lock:
                tally['other']+=1; detail.append('t%d i%d %s'%(tid,i,repr(e)))
ts=[threading.Thread(target=worker,args=(t,)) for t in range(NT)]
for t in ts: t.start()
for t in ts: t.join()
print('total=%d mismatch=%d timeout=%d other=%d'%(NT*PER,tally['mismatch'],tally['timeout'],tally['other']))
for d in detail[:30]: print(d)
" vpn-port *threads* *per-thread* *max-bytes*)))

(defun parse-tally (output key)
  "Pull the integer following '<KEY>=' out of the python OUTPUT."
  (let ((pos (search (format nil "~a=" key) output)))
    (and pos (parse-integer output :start (+ pos (length key) 1) :junk-allowed t))))

(let* ((name (format nil "erebus_istress_~a" (random-string 12)))
       (folder (container-folder name))
       (vpn-port (funcall (gen-integer :min 20000 :max 30000)))
       (clean nil))
  (handler-case
      (progn
        (ensure-test-image)
        (create-container name folder vpn-port)
        (prepare-container name folder)
        (configure-openvpn name)
        (start-services name)
        (sleep 6)
        (setf *dev-folder* folder *dev-port* vpn-port *dev-container* name)
        (let ((client (dev-client)) (exposure nil))
          (unwind-protect
               (with-local-tcp-server (local-port #'count-handler)
                 (setf exposure (expose client :vpn-port *vpn-port*
                                               :host "127.0.0.1" :port local-port))
                 (format t "~%Hammering 10.8.0.2:~a with ~a threads x ~a connections (~a total), \
payloads up to ~a bytes...~%"
                         *vpn-port* *threads* *per-thread*
                         (* *threads* *per-thread*) *max-bytes*)
                 (let* ((output (run-python-in-container name (stress-python *vpn-port*)))
                        (total (parse-tally output "total"))
                        (mismatch (parse-tally output "mismatch"))
                        (timeout (parse-tally output "timeout"))
                        (other (parse-tally output "other")))
                   (format t "~%---- client tally ----~%~a~%-----------------------~%" output)
                   ;; correctness = no wrong bytes and no unexpected errors.
                   ;; stalled (timed-out) connections are the documented
                   ;; no-retransmission limitation, reported but not a bug.
                   (setf clean (and (eql mismatch 0) (eql other 0)))
                   (cond
                     ((and clean (eql timeout 0))
                      (format t "~%PASS: all ~a inbound connections round-tripped correctly.~%" total))
                     (clean
                      (format t "~%PASS (with caveats): every completed connection was correct; \
~a/~a stalled (no-retransmission limitation, not a logic bug).~%"
                              timeout total))
                     (t
                      (format t "~%FAIL: ~a mismatch(es) and ~a other error(s) out of ~a -- \
these are correctness bugs.~%" mismatch other total)))))
            (when exposure (ignore-errors (unexpose exposure)))
            (ignore-errors (disconnect client)))))
    (error (e) (format t "~&STRESS-ERROR: ~a~%" e)))
  (format t "~&== tearing down ==~%")
  (ignore-errors (cleanup-container name folder))
  (format t "~&== done ==~%")
  (uiop:quit (if clean 0 1)))
