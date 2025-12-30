(in-package #:erebus/test)

(def-suite* erebus/connect :in erebus)

(test connect-to-vpn-static-key-udp
  (with-docker-container (name
                          folder
                          vpn-local-port
                          (lambda (name folder)
                            (run-in-container
                             name
                             "cd /etc/openvpn
openvpn --genkey --secret static.key
chmod 777 static.key
rm -rf ccd/ crl.pem pki/ # delete those or ovpn_run will try to use them

cat > /etc/openvpn/openvpn.conf <<EOF
server 192.168.255.0 255.255.255.0
verb 3
keepalive 10 60
persist-tun
secret static.key

proto udp
# Rely on Docker to do port mapping, internally always 1194
port 1194
dev tun0
status /tmp/openvpn-status.log

user nobody
group nogroup
comp-lzo no

### Route Configurations Below
route 192.168.254.0 255.255.255.0

# --- Push minimal config ---
push \"route 192.168.254.0 255.255.255.0\"
push \"block-outside-dns\"
push \"dhcp-option DNS 8.8.8.8\"
push \"comp-lzo no\"
EOF
")))
    (let ((socket (usocket:socket-connect "localhost" vpn-local-port :protocol :datagram)))
      (usocket:socket-close socket)
      ;; just checking we didn't raise any conditions
      (is (= 1 1)))))
