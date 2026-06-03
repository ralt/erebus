LISP ?= sbcl

all: erebus

erebus: $(wildcard src/*.lisp) erebus.asd
	$(LISP) --eval '(ql:quickload :erebus)' \
		--eval '(asdf:make :erebus)' \
		--eval '(quit)'

# Run the automated test suite (requires docker; spins up openvpn containers).
test:
	$(LISP) --non-interactive \
		--eval '(ql:quickload :erebus/test)' \
		--eval '(asdf:test-system :erebus)' \
		--eval '(quit)'

# One-shot manual smoke test of the HTTP proxy against a throwaway container.
verify:
	$(LISP) --script t/manual-verify.lisp

# Performance comparison of the proxy against tinyproxy and a direct fetch.
stress:
	$(LISP) --script t/stress.lisp

# Correctness stress test of inbound port-forwarding: many concurrent
# connections from the VPN side, verifying every transfer round-trips.
stress-inbound:
	$(LISP) --script t/stress-inbound.lisp

# Read the man page formatted, straight from the source tree.
man:
	man ./doc/erebus.1

# Build the native Debian package with linux-packaging, in a throwaway
# container that matches the release CI. Requires docker; the .deb lands in
# ./dist/. Pass VERSION=x.y.z to stamp a version (defaults to 1.0.0). NB: the
# container installs a toolchain and compiles the Lisp dependencies, so it
# takes a few minutes the first time.
DOCKER ?= docker

package-deb:
	$(DOCKER) run --rm -e VERSION=$(VERSION) -v "$(CURDIR)":/src -w /src debian:stable ./.ci/build.sh

.PHONY: test verify stress stress-inbound man package-deb
