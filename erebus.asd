(defsystem erebus
  :serial t
  :license "GPLv2"
  :author "Florian Margaine <florian@margaine.com>"
  :description "Rootless IPSec userspace proxy"
  :depends-on (:uiop
               :alexandria
               :usocket)
  :in-order-to ((test-op (test-op :erebus/test)))
  :components ((:module "src"
                :serial t
                :components ((:file "package")))))

(defsystem erebus/test
  :depends-on (:fiveam)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "connect"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :erebus :erebus/test))))
