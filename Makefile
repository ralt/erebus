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

.PHONY: test verify stress stress-inbound
