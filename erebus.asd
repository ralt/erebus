(defsystem erebus
  :serial t
  :license "GPLv2"
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
               :cl-ini)
  :in-order-to ((test-op (test-op :erebus/test)))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "ip")
                             (:file "vpn-connection")
                             (:file "openvpn")
                             (:file "main")))))

(defsystem erebus/test
  :depends-on (:erebus
               :fiveam
               :alexandria)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "connect")
                             (:file "openvpn-statickey"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :erebus :erebus/test))))
