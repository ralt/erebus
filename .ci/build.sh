#!/usr/bin/env bash
#
# Build a native Linux package for erebus using linux-packaging
# (https://gitlab.com/ralt/linux-packaging), via FPM.
#
# linux-packaging's build-op extends cffi-toolchain's static-program-op, which
# links the SBCL runtime (sbcl.o + sbcl.mk) together with the dumped core into
# a standalone executable. Modern distro SBCL packages ship those linkable-
# runtime artifacts, so -- unlike linux-packaging's original CI -- we do NOT
# need to compile SBCL from source; the stock package is enough. (We still need
# libzstd at link time because the runtime is built with core compression.)
#
# Usage:   .ci/build.sh <deb|rpm|pacman>
# Output:  the built package is copied into ./dist/
#
# Designed to run as root inside a throwaway distro container -- that is how
# both the GitHub Actions release workflow and `make package-*` drive it.

set -xeuo pipefail

# Some base images run as root without HOME set; we write quicklisp, gems and
# clones under it, and `set -u` would otherwise abort on an unset $HOME.
export HOME="${HOME:-/root}"

t="${1:?usage: build.sh <deb|rpm|pacman>}"

# linux-packaging picks this up; default mirrors its own default.
export VERSION="${VERSION:-1.0.0}"

repo="$PWD"

# FPM writes the package into the working directory and refuses to overwrite an
# existing one, so clear any artifact left by a previous run before we start.
rm -f "$repo"/*.deb "$repo"/*.rpm "$repo"/*.pkg.tar* 2>/dev/null || true

# ---------------------------------------------------------------------------
# 1. Distro dependencies: SBCL (with its linkable-runtime artifacts), a C
#    toolchain + libzstd for the final relink, FPM's ruby, and nodejs so the
#    GitHub Actions steps can run inside these minimal images.
# ---------------------------------------------------------------------------
case "$t" in
    deb)
        apt-get update -qq
        apt-get install -y --no-install-recommends \
            ruby ruby-dev rubygems dpkg-dev sbcl curl git ca-certificates \
            zlib1g-dev libzstd-dev build-essential nodejs
        ;;
    rpm)
        dnf -y group install "Development Tools" || dnf -y groupinstall "Development Tools"
        dnf install -y \
            ruby ruby-devel rubygems rpm-build sbcl libffi-devel \
            redhat-rpm-config git curl zlib-devel libzstd-devel nodejs
        ;;
    pacman)
        pacman -Sy --noconfirm \
            ruby rubygems sbcl git libffi gcc make curl which zstd nodejs
        ;;
    *)
        echo "unknown package type: $t (expected deb, rpm or pacman)" >&2
        exit 2
        ;;
esac

# ---------------------------------------------------------------------------
# 2. FPM (the packager linux-packaging shells out to).
# ---------------------------------------------------------------------------
gem install --no-document fpm
# gem may install to a per-user dir or a system one depending on the distro;
# add the per-user gem bindir to PATH when it exists, then make sure fpm is
# actually reachable.
if [ -d "$HOME/.gem/ruby" ]; then
    export PATH="$HOME/.gem/ruby/$(ls "$HOME/.gem/ruby" | head -1)/bin:$PATH"
fi
command -v fpm >/dev/null || { echo "fpm not found on PATH after install" >&2; exit 1; }

# Point SBCL_HOME at the directory holding the distro's sbcl.core. The
# static-program-op relink runs a freshly-linked, bare runtime against that
# core to dump the executable; unlike the packaged `sbcl`, that bare runtime
# has no built-in home, so without SBCL_HOME it cannot (require "asdf") and the
# dump fails. uiop:run-program passes our environment through to it.
SBCL_HOME="$(dirname "$(sbcl --noinform --non-interactive \
    --eval '(progn (princ (namestring sb-ext:*core-pathname*)) (terpri))' 2>/dev/null | tail -1)")"
export SBCL_HOME
echo "using SBCL_HOME=$SBCL_HOME"

# ---------------------------------------------------------------------------
# 3. Quicklisp.
# ---------------------------------------------------------------------------
curl -O https://beta.quicklisp.org/quicklisp.lisp
# Install into ~/quicklisp. We deliberately do NOT call (ql:add-to-init-file):
# it prompts "Press Enter to continue" and would hit EOF here, and we never
# need it -- every invocation below loads ~/quicklisp/setup.lisp explicitly.
sbcl --non-interactive \
     --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)'
rm -f quicklisp.lisp

# ---------------------------------------------------------------------------
# 4. linux-packaging and the recent ASDF/CFFI it needs, on the source registry
#    alongside this checkout of erebus.
# ---------------------------------------------------------------------------
mkdir -p ~/common-lisp
git clone --depth=1 https://github.com/privet-kitty/wild-package-inferred-system.git ~/common-lisp/wild-package-inferred-system
git clone --depth=1 https://github.com/cffi/cffi.git ~/common-lisp/cffi
git clone --depth=1 https://gitlab.common-lisp.net/asdf/asdf.git ~/common-lisp/asdf
git clone --depth=1 https://gitlab.com/ralt/linux-packaging.git ~/common-lisp/linux-packaging

mkdir -p ~/.config/common-lisp/source-registry.conf.d
{
    printf '(:tree "%s/")\n' "$repo"
    printf '(:tree "%s/common-lisp/")\n' "$HOME"
} > ~/.config/common-lisp/source-registry.conf.d/erebus.conf

# A debugger hook so a load/compile failure exits non-zero instead of hanging.
die_hook='(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~&FATAL: ~A~%" c) (sb-ext:exit :code 1)))'

# ---------------------------------------------------------------------------
# 5. Build the package. First pull every dependency through Quicklisp, then in
#    a fresh image dump the standalone binary and let FPM wrap it. (Two
#    invocations, as in linux-packaging's own CI: a clean image is dumped.)
# ---------------------------------------------------------------------------
sbcl --non-interactive \
     --eval "$die_hook" \
     --load ~/quicklisp/setup.lisp \
     --eval '(ql:quickload :linux-packaging)' \
     --eval '(asdf:load-asd (truename "erebus-packaging.asd"))' \
     --eval '(ql:quickload :erebus)'

sbcl --non-interactive \
     --eval "$die_hook" \
     --load ~/quicklisp/setup.lisp \
     --eval '(ql:quickload :linux-packaging)' \
     --eval '(asdf:load-asd (truename "erebus-packaging.asd"))' \
     --eval "(asdf:make \"erebus-packaging/$t\")"

# ---------------------------------------------------------------------------
# 6. Collect the artifact.
# ---------------------------------------------------------------------------
mkdir -p dist
find "$repo" "$HOME" -maxdepth 3 -type f \
     \( -name '*.deb' -o -name '*.rpm' -o -name '*.pkg.tar*' \) \
     -exec cp -v {} dist/ \;

echo "Built packages:"
ls -l dist/
