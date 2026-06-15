(in-package #:erebus/test)

(def-suite* erebus/openvpn-statickey :in erebus)

(defun nginx-404-p (body status)
  "True when BODY/STATUS look like the 404 nginx serves for \"/\" (see
t/nginx.conf). We match on content rather than the exact bytes so the
test is not coupled to nginx's error-page whitespace across versions."
  (and (eql status 404)
       (stringp body)
       (search "404 Not Found" body)
       (search "nginx" body)))

(test ping-statickey
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (ping client "10.8.0.1")
      ;; if we reach here, it means we didn't raise nor blocked
      (is (= 1 1)))))

(test ping-statickey-key-direction-normal
  (with-docker-container (name folder vpn-local-port
                               (openvpn-prep-hook :secret "secret static.key 0"))
    (with-test-client (client folder vpn-local-port :key-direction "1")
      (ping client "10.8.0.1")
      (is (= 1 1)))))

(test ping-statickey-tcp
  (with-docker-container (name folder vpn-local-port
                               (openvpn-prep-hook :proto "tcp-server"
                                                  :secret "secret static.key 0"))
    (with-test-client (client folder vpn-local-port :protocol :stream :key-direction "1")
      (ping client "10.8.0.1")
      (is (= 1 1)))))

(test ping-statickey-minimal-tcp-request
  (with-docker-container (name folder vpn-local-port
                               (openvpn-prep-hook :pre "nohup echo-server &"))
    (with-test-client (client folder vpn-local-port)
      (let* ((socket (socket-connect client :protocol :stream :host "10.8.0.1" :port 9999))
             (stream (socket-stream socket))
             (out (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(1))))
        (write-sequence out stream)
        (finish-output stream)
        (let ((in (make-array 1 :element-type '(unsigned-byte 8))))
          (read-sequence in stream)
          (is (= 1 (elt in 0))))
        ;; orderly close: echo-server sees our FIN, closes, we complete
        ;; the handshake without erroring.
        (finishes (socket-close socket))))))

(test ping-statickey-minimal-http-proxy
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (with-proxy (proxy-port client)
        (multiple-value-bind (body status)
            (drakma:http-request "http://10.8.0.1"
                                 :proxy `("127.0.0.1" ,proxy-port)
                                 :keep-alive t
                                 :close nil)
          (is (nginx-404-p body status)))))))

(test tcp-large-outbound-write
  ;; A write larger than a single TCP segment must be split into several
  ;; segments: one oversized IP packet exceeds the VPN tun MTU and is
  ;; dropped on the wire. The count-server replies with the number of
  ;; payload bytes it received; we check it matches what we sent, for both
  ;; a single-segment payload and a clearly multi-segment one.
  (with-docker-container (name folder vpn-local-port
                               (openvpn-prep-hook :pre "nohup count-server &"))
    (with-test-client (client folder vpn-local-port)
      (dolist (size '(1000 20000))
        (let* ((socket (socket-connect client :protocol :stream :host "10.8.0.1" :port 9998))
               (stream (socket-stream socket)))
          (unwind-protect
               (progn
                 (write-sequence (uint32-be size) stream)
                 (write-sequence (make-array size :element-type '(unsigned-byte 8)
                                                   :initial-element 120)
                                 stream)
                 (finish-output stream)
                 (is (= size (parse-integer (read-line-from-octet-stream stream)))))
            (socket-close socket)))))))

(test http-proxy-post-body
  ;; The proxy must forward request bodies, including ones larger than a
  ;; single TCP segment. http-echo-server replies with the number of body
  ;; bytes it received; we POST both a small and a large body and check.
  (with-docker-container (name folder vpn-local-port
                               (openvpn-prep-hook :pre "nohup http-echo-server &"))
    (with-test-client (client folder vpn-local-port)
      (with-proxy (proxy-port client)
        (dolist (size '(500 20000))
          (multiple-value-bind (body status)
              (drakma:http-request "http://10.8.0.1:8000/"
                                   :proxy `("127.0.0.1" ,proxy-port)
                                   :method :post
                                   :content (make-string size :initial-element #\x)
                                   :keep-alive nil :close t)
            (is (= 200 status))
            (is (= size (parse-integer body)))))))))

(test http-proxy-fragmented-response
  ;; A response larger than a single ~1.4kB TCP segment must be
  ;; reassembled across several segments. We serve a deterministic 20000
  ;; byte file (Content-Length framed, nginx keeps the connection alive)
  ;; through /man/ and check we get every byte back.
  (let ((big-size 20000))
    (with-docker-container
        (name folder vpn-local-port
              (openvpn-prep-hook
               :pre (format nil "python3 -c \"open('/usr/share/man/big.txt','w').write('A'*~a)\""
                            big-size)))
      (with-test-client (client folder vpn-local-port)
        (with-proxy (proxy-port client)
          (multiple-value-bind (body status)
              (drakma:http-request "http://10.8.0.1/man/big.txt"
                                   :proxy `("127.0.0.1" ,proxy-port)
                                   :keep-alive t
                                   :close nil)
            (is (= 200 status))
            (is (= big-size (length body)))
            (is (every (lambda (c) (char= c #\A)) body))))))))

(test http-proxy-connection-close
  ;; With "Connection: close" nginx sends the body and then FINs. The
  ;; proxy must read the whole body and tear the connection down cleanly.
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (with-proxy (proxy-port client)
        (multiple-value-bind (body status)
            (drakma:http-request "http://10.8.0.1"
                                 :proxy `("127.0.0.1" ,proxy-port)
                                 :close t)
          (is (nginx-404-p body status)))))))

(test http-proxy-sequential-requests
  ;; Each proxied request opens (and closes) its own VPN-side TCP
  ;; connection. Issuing several in a row exercises connection setup and
  ;; teardown repeatedly without leaking or hanging.
  (with-docker-container (name folder vpn-local-port (openvpn-prep-hook))
    (with-test-client (client folder vpn-local-port)
      (with-proxy (proxy-port client)
        (dotimes (i 3)
          (multiple-value-bind (body status)
              (drakma:http-request "http://10.8.0.1"
                                   :proxy `("127.0.0.1" ,proxy-port)
                                   :keep-alive t
                                   :close nil)
            (is (nginx-404-p body status))))))))

(test https-connect-tunnelling
  ;; The proxy must handle CONNECT to tunnel opaque TLS bytes to a VPN
  ;; resource. nginx on the VPN side gets a self-signed cert on port 443;
  ;; drakma issues CONNECT, the proxy relays the TLS handshake and data
  ;; transparently. We check both a small response (404) and a clearly
  ;; multi-segment one (a 20000 byte file) round-trip through the tunnel.
  (let ((big-size 20000))
    (with-docker-container
        (name folder vpn-local-port
              (lambda (name folder)
                (declare (ignore folder))
                (configure-openvpn name)
                (run-in-container
                 name
                 "openssl req -x509 -newkey rsa:2048 -keyout /etc/nginx/server.key -out /etc/nginx/server.crt -days 365 -nodes -subj /CN=localhost")
                (run-in-container
                 name
                 (format nil "python3 -c \"open('/usr/share/man/big.txt','w').write('A'*~a)\""
                         big-size))
                (run-python-in-container
                 name
                 "with open('/etc/nginx/conf.d/https.conf', 'w') as f:
    f.write('''server {
    listen 443 ssl;
    ssl_certificate /etc/nginx/server.crt;
    ssl_certificate_key /etc/nginx/server.key;
    location / {
        return 404;
    }
    location /man/ {
        alias /usr/share/man/;
    }
}
''')")))
      (with-test-client (client folder vpn-local-port)
        (with-proxy (proxy-port client)
          ;; small response: proves the TLS handshake + request + response
          ;; all relay through the tunnel.
          (multiple-value-bind (body status)
              (drakma:http-request "https://10.8.0.1/"
                                   :proxy `("127.0.0.1" ,proxy-port)
                                   :verify nil
                                   :keep-alive t
                                   :close nil)
            (is (nginx-404-p body status)))
          ;; large response: exercises the relay across many TCP segments.
          (multiple-value-bind (body status)
              (drakma:http-request "https://10.8.0.1/man/big.txt"
                                   :proxy `("127.0.0.1" ,proxy-port)
                                   :verify nil
                                   :keep-alive t
                                   :close nil)
            (is (= 200 status))
            (is (= big-size (length body)))
            (is (every (lambda (c) (char= c #\A)) body))))))))
