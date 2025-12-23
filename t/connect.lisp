(in-package #:erebus/test)

(def-suite* erebus/connect :in erebus)

(test connect-to-vpn
  (with-docker-container (name folder)
    (sleep 1)))
