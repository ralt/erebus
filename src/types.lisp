(in-package #:erebus)

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype octet ()
  '(unsigned-byte 8))
