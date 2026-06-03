(defsystem erebus
  :serial t
  :license "GPLv3"
  :author "Florian Margaine <florian@margaine.com>"
  :description "Rootless VPN userspace proxy"
  :depends-on (:usocket
               :lisp-binary
               :ironclad
               :flexi-streams
               :uiop
               :bordeaux-threads
               :lparallel
               :local-time
               :clingon
               :cl-ini
               :alexandria
               :trivial-gray-streams
               :hunchentoot
               :cl-ppcre)
  :in-order-to ((test-op (test-op :erebus/test)))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "ip")
                             (:file "vpn-connection")
                             (:file "data-plane")
                             (:file "openvpn")
                             (:file "ikev2")
                             (:file "socket")
                             (:file "http")
                             (:file "main"))))
  :build-operation "program-op"
  :build-pathname "erebus"
  :entry-point "erebus::main")

(defsystem erebus/test
  :depends-on (:erebus
               :fiveam
               :drakma)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "connect")
                             (:file "ikev2")
                             (:file "openvpn-statickey")
                             (:file "expose"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :erebus :erebus/test))))
