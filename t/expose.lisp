(in-package #:erebus/test)

(def-suite* erebus/expose :in erebus)

;;; Phase 6: exposing local services to the VPN. These mirror the outbound
;;; tests but in the opposite direction: the *backend service runs on the
;;; host* (WITH-LOCAL-TCP-SERVER) and the *client runs inside the container*
;;; (a python3 one-liner), connecting to the erebus client's VPN IP
;;; (10.8.0.2) on an exposed port. erebus accepts the inbound connection and
;;; relays it to the local service.

(defparameter +py-preamble+
  "import socket,struct,sys,time
def conn(port):
    last=None
    for _ in range(40):
        try:
            return socket.create_connection(('10.8.0.2',port),timeout=30)
        except OSError as e:
            last=e; time.sleep(0.5)
    raise SystemExit('connect failed: %s'%last)
"
  "Python helper run inside the container: conn(port) opens a connection to
the erebus client IP, retrying briefly so a not-quite-ready tunnel doesn't
flake the test.")

(test expose-inbound-echo
  ;; Proves the passive open (we accept a peer-initiated connection) plus a
  ;; bidirectional relay and orderly teardown: the peer sends a few bytes
  ;; and gets them echoed back by a host-side service.
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (with-local-tcp-server (local-port
                              (lambda (stream)
                                (loop for b = (read-byte stream nil :eof)
                                      until (eq b :eof)
                                      do (write-byte b stream)
                                         (force-output stream))))
        (let ((exposure (expose client :vpn-port 8080 :host "127.0.0.1" :port local-port)))
          (unwind-protect
               (is (string= "hello"
                            (run-python-in-container
                             name
                             (concatenate 'string +py-preamble+ "
s=conn(8080)
s.sendall(b'hello')
d=b''
while len(d)<5:
    c=s.recv(5-len(d))
    if not c: break
    d+=c
s.close()
sys.stdout.write(d.decode())
"))))
            (unexpose exposure)))))))

(test expose-inbound-large-request
  ;; A request larger than a single TCP segment must be read across several
  ;; segments on the inbound (accepted) side. The host count-server reads a
  ;; 4-byte big-endian length then that many bytes, and replies with the
  ;; count; we send both a single-segment and a multi-segment payload.
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (with-local-tcp-server (local-port
                              (lambda (stream)
                                (let ((n (be32-to-integer (read-n-octets stream 4))))
                                  (read-n-octets stream n)
                                  (write-sequence (babel:string-to-octets (format nil "~a~%" n))
                                                  stream)
                                  (force-output stream))))
        (let ((exposure (expose client :vpn-port 8081 :host "127.0.0.1" :port local-port)))
          (unwind-protect
               (dolist (n '(1000 20000))
                 (is (= n (parse-integer
                           (run-python-in-container
                            name
                            (concatenate 'string +py-preamble+
                                         (format nil "
N=~a
s=conn(8081)
s.sendall(struct.pack('>I',N)+b'x'*N)
line=b''
while not line.endswith(b'\\n'):
    c=s.recv(100)
    if not c: break
    line+=c
s.close()
sys.stdout.write(line.decode().strip())
" n)))))))
            (unexpose exposure)))))))

(test expose-inbound-large-response
  ;; A response larger than a single TCP segment must be segmented on the
  ;; inbound (accepted) side's write path. The host server writes back N
  ;; bytes and closes; the peer reads to EOF and reports the total.
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (with-local-tcp-server (local-port
                              (lambda (stream)
                                (let ((n (be32-to-integer (read-n-octets stream 4))))
                                  (write-sequence (make-array n :element-type '(unsigned-byte 8)
                                                                :initial-element 65)
                                                  stream)
                                  (force-output stream))))
        (let ((exposure (expose client :vpn-port 8082 :host "127.0.0.1" :port local-port)))
          (unwind-protect
               (dolist (n '(1000 20000))
                 (is (= n (parse-integer
                           (run-python-in-container
                            name
                            (concatenate 'string +py-preamble+
                                         (format nil "
N=~a
s=conn(8082)
s.sendall(struct.pack('>I',N))
total=0
while True:
    c=s.recv(65536)
    if not c: break
    total+=len(c)
s.close()
sys.stdout.write(str(total))
" n)))))))
            (unexpose exposure)))))))
