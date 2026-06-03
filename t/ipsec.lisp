(in-package #:erebus/test)

;;; Phase 9: live IKEv2/ESP interoperability with strongSwan. These mirror the
;;; OpenVPN integration tests (t/openvpn-statickey.lisp, t/expose.lisp) but
;;; over the IPsec data plane. The server's in-tunnel nginx answers on
;;; 10.10.0.1; our client is assigned 10.9.0.1 out of strongSwan's pool.

(def-suite* erebus/ipsec :in erebus)

(test ping-ipsec
  ;; The whole control plane in one shot: IKE_SA_INIT + IKE_AUTH (PSK) bring
  ;; up the CHILD_SA, then an ESP-encapsulated ICMP echo round-trips.
  (with-ipsec-container (name ike natt)
    (with-ipsec-test-client (client ike natt)
      (ping client +ipsec-server-ip+)
      ;; reaching here means the ping was answered without error or hang.
      (is (= 1 1)))))

(test ipsec-tcp-raw-request
  ;; A bare TCP request over ESP: open a connection through the tunnel to
  ;; nginx, send a request line, and read the status line back.
  (with-ipsec-container (name ike natt)
    (with-ipsec-test-client (client ike natt)
      (let* ((socket (socket-connect client :protocol :stream
                                            :host +ipsec-server-ip+ :port 80))
             (stream (socket-stream socket)))
        (unwind-protect
             (progn
               (write-sequence (babel:string-to-octets
                                (format nil "GET / HTTP/1.0~c~c~c~c"
                                        #\return #\linefeed #\return #\linefeed))
                               stream)
               (finish-output stream)
               (is (search "404" (read-line-from-octet-stream stream))))
          (socket-close socket))))))

(test ipsec-http-proxy
  ;; The outbound HTTP proxy over ESP: drakma -> erebus proxy -> nginx.
  (with-ipsec-container (name ike natt)
    (with-ipsec-test-client (client ike natt)
      (with-proxy (proxy-port client)
        (multiple-value-bind (body status)
            (drakma:http-request (format nil "http://~a" +ipsec-server-ip+)
                                 :proxy `("127.0.0.1" ,proxy-port)
                                 :keep-alive t
                                 :close nil)
          (is (nginx-404-p body status)))))))

(test ipsec-http-proxy-fragmented-response
  ;; A response larger than a single TCP segment must be reassembled across
  ;; segments over ESP, exactly as for OpenVPN.
  (let ((big-size 20000))
    (with-ipsec-container (name ike natt)
      ;; nginx's /man/ aliases /usr/share/man/; drop a deterministic big file.
      (run-in-container
       name
       (format nil "python3 -c \"open('/usr/share/man/big.txt','w').write('A'*~a)\""
               big-size))
      (with-ipsec-test-client (client ike natt)
        (with-proxy (proxy-port client)
          (multiple-value-bind (body status)
              (drakma:http-request (format nil "http://~a/man/big.txt" +ipsec-server-ip+)
                                   :proxy `("127.0.0.1" ,proxy-port)
                                   :keep-alive t
                                   :close nil)
            (is (= 200 status))
            (is (= big-size (length body)))
            (is (every (lambda (c) (char= c #\A)) body))))))))

(defparameter +ipsec-py-preamble+
  "import socket,struct,sys,time
def conn(port):
    last=None
    for _ in range(40):
        try:
            return socket.create_connection(('10.9.0.1',port),timeout=30,source_address=('10.10.0.1',0))
        except OSError as e:
            last=e; time.sleep(0.5)
    raise SystemExit('connect failed: %s'%last)
"
  "Python helper run inside the strongSwan container: conn(port) opens a
connection to the erebus client's assigned IP (10.9.0.1), sourced from the
server's in-tunnel address (10.10.0.1) so it matches the CHILD_SA policy.")

(test ipsec-expose-inbound-echo
  ;; Inbound (peer-initiated) connection over ESP: a passive open plus a
  ;; bidirectional relay to a host-side echo service.
  (with-ipsec-container (name ike natt)
    (with-ipsec-test-client (client ike natt)
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
                             (concatenate 'string +ipsec-py-preamble+ "
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
