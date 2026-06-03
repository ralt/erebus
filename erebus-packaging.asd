;;;; Native Debian package definition for erebus.
;;;;
;;;; Kept out of erebus.asd on purpose: it pulls in linux-packaging
;;;; (https://gitlab.com/ralt/linux-packaging) and FPM, neither of which is
;;;; needed to use or hack on erebus itself. It is only loaded at release time,
;;;; by .ci/build.sh (which the GitHub Actions release workflow and the
;;;; `make package-deb` target both drive).
;;;;
;;;; Build it with:  sbcl --eval '(asdf:make "erebus-packaging/deb")'
;;;; The package version comes from the VERSION environment variable.
;;;;
;;;; The system is named "erebus-packaging/deb" (a secondary system of the
;;;; primary "erebus-packaging") to satisfy ASDF's rule that a foo.asd file
;;;; only define system "foo" and names starting with "foo/". The produced
;;;; package is still called "erebus" (see :package-name).

;; Trivial primary system so the file name matches a defined system.
(defsystem "erebus-packaging"
  :description "Meta-system; build erebus-packaging/deb for the native .deb.")

(defsystem "erebus-packaging/deb"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:deb"
  :build-operation "linux-packaging:build-op"
  :depends-on ("erebus")
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :description "Rootless, user-space VPN proxy for OpenVPN"
  :homepage "https://github.com/ralt/erebus"
  :package-name "erebus"
  :build-pathname "erebus"
  :entry-point "erebus::main"
  ;; Ship the man page and an annotated config alongside the binary. FPM (dir
  ;; source) treats each destination as a directory and drops the source file
  ;; in under its own basename, so these are the target *directories*.
  :additional-files (("doc/erebus.1" . "/usr/share/man/man1/")
                     ("default-config.ini" . "/usr/share/doc/erebus/")
                     ("README.md" . "/usr/share/doc/erebus/")
                     ("LICENSE.md" . "/usr/share/doc/erebus/")))
