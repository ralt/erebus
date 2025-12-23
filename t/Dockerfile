# We're taking the docker image from
# https://github.com/kylemanna/docker-openvpn and adding nginx in it
# so that we have a basic http server available on the VPN subnet.
# Our test will be trying to reach that http server over erebus
# running as a VPN proxy.

FROM kylemanna/openvpn

RUN apk add nginx

CMD ["bash", "-c", "sleep infinity"]
