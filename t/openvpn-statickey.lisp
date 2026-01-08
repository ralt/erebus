(in-package #:erebus/test)

(def-suite* erebus/openvpn-statickey :in erebus)

(test ping-statickey
  (with-docker-container (name
                          folder
                          vpn-local-port
                          (lambda (name folder)
                            (declare (ignore folder))
                            (run-in-container
                             name
                             "cd /etc/openvpn
openvpn --genkey --secret static.key
chmod 777 static.key
rm -rf ccd/ crl.pem pki/ # delete those or ovpn_run will try to use them

cat > /etc/openvpn/openvpn.conf <<EOF
ifconfig 10.8.0.1 10.8.0.2
verb 9
keepalive 10 60
secret static.key
cipher AES-256-CBC
auth SHA256

proto udp
port 1194
dev tun0
persist-tun
status /tmp/openvpn-status.log
log /etc/openvpn/openvpn.log
user nobody
group nogroup
comp-lzo no
EOF
")))
    (let ((openvpn-client (make-instance 'openvpn-client-static-key
                                         :host "localhost"
                                         :port vpn-local-port
                                         :client-ip "10.8.0.2"
                                         :secret (namestring
                                                  (make-pathname
                                                   :name "static.key"
                                                   :directory (pathname-directory folder)))
                                         :cipher "AES-256-CBC"
                                         :auth "SHA256")))
      (connect openvpn-client)
      (unwind-protect
           (progn
             (ping openvpn-client "10.8.0.1")
             (is (= 1 1))) ; if we reach here, it means we didn't raise nor blocked
        (disconnect openvpn-client)))))

(test ping-statickey-key-direction-normal
  (with-docker-container (name
                          folder
                          vpn-local-port
                          (lambda (name folder)
                            (declare (ignore folder))
                            (run-in-container
                             name
                             "cd /etc/openvpn
openvpn --genkey --secret static.key
chmod 777 static.key
rm -rf ccd/ crl.pem pki/ # delete those or ovpn_run will try to use them

cat > /etc/openvpn/openvpn.conf <<EOF
ifconfig 10.8.0.1 10.8.0.2
verb 9
keepalive 10 60
secret static.key 0
cipher AES-256-CBC
auth SHA256

proto udp
port 1194
dev tun0
persist-tun
status /tmp/openvpn-status.log
log /etc/openvpn/openvpn.log
user nobody
group nogroup
comp-lzo no
EOF
")))
    (let ((openvpn-client (make-instance 'openvpn-client-static-key
                                         :host "localhost"
                                         :port vpn-local-port
                                         :client-ip "10.8.0.2"
                                         :secret (namestring
                                                  (make-pathname
                                                   :name "static.key"
                                                   :directory (pathname-directory folder)))
                                         :key-direction "1"
                                         :cipher "AES-256-CBC"
                                         :auth "SHA256")))
      (connect openvpn-client)
      (unwind-protect
           (progn
             (ping openvpn-client "10.8.0.1")
             (is (= 1 1))) ; if we reach here, it means we didn't raise nor blocked
        (disconnect openvpn-client)))))
