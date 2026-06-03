# We're taking the docker image from
# https://github.com/kylemanna/docker-openvpn and adding nginx in it
# so that we have a basic http server available on the VPN subnet.
# Our test will be trying to reach that http server over erebus
# running as a VPN proxy.

FROM kylemanna/openvpn

RUN apk add nginx python3 man-pages tinyproxy curl

COPY nginx.conf /etc/nginx/conf.d/default.conf

# We slightly modified easyrsa to not prompt during CA generation. The
# current upstream has no way to define a CN non-interactively.
COPY easyrsa /usr/share/easy-rsa/easyrsa
RUN chmod +x /usr/share/easy-rsa/easyrsa

# easy echo server to deal with
COPY echo-server /usr/bin/echo-server
RUN chmod +x /usr/bin/echo-server

# server that counts the bytes of a length-prefixed payload, used to test
# large outbound writes (TCP segmentation of requests)
COPY count-server /usr/bin/count-server
RUN chmod +x /usr/bin/count-server

# HTTP server that echoes the received request-body length, used to test
# the proxy's request path (including large POST bodies)
COPY http-echo-server /usr/bin/http-echo-server
RUN chmod +x /usr/bin/http-echo-server

CMD ["bash", "-c", "sleep infinity"]
