(in-package #:erebus/test)

(def-suite* erebus/connect :in erebus)

(test connect-to-vpn-static-key-udp
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
verb 3
keepalive 10 60
persist-tun
secret static.key
cipher AES-256-CBC
auth SHA256

proto udp
port 1194
dev tun0
status /tmp/openvpn-status.log
log /etc/openvpn/openvpn.log
user nobody
group nogroup
comp-lzo no
EOF
")))
    (let ((socket (usocket:socket-connect "localhost" vpn-local-port :protocol :datagram)))
      (usocket:socket-close socket)
      ;; just checking we didn't raise any conditions
      (is (= 1 1)))))
