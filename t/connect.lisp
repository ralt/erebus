(in-package #:erebus/test)

(def-suite* erebus/connect :in erebus)

(test connect-to-vpn
  (with-docker-container (name
                          folder
                          vpn-local-port
                          (lambda (name folder)
                            (run-in-container
                             name
                             "cat > /etc/openvpn/openvpn.conf <<EOF
ca /etc/openvpn/pki/ca.crt

port 1194
proto tcp
dev tun0

user nobody
group nogroup
comp-lzo no

mode server
server 192.168.255.0 255.255.255.0
route 192.168.254.0 255.255.255.0

# --- Disable client authentication ---
verify-client-cert none
auth-user-pass-verify /bin/true via-file
username-as-common-name
auth none
duplicate-cn

# --- Crypto (still required internally) ---
dh none
cipher AES-256-GCM
ncp-disable
keysize 256

# --- Control channel ---
keepalive 10 60
persist-key
persist-tun

# --- Push minimal config ---
push \"route 192.168.254.0 255.255.255.0\"
push \"block-outside-dns\"
push \"dhcp-option DNS 8.8.8.8\"
push \"comp-lzo no\"

# --- Logging ---
verb 4
log /etc/openvpn/openvpn.log
status /tmp/openvpn.status
")))
    (let ((socket (usocket:socket-connect "localhost" vpn-local-port)))
      (usocket:socket-close socket)
      ;; just checking we didn't raise any conditions
      (is (= 1 1)))))
