(defpackage :cl-smt
  (:use
   :common-lisp
   :alexandria
   :cl-arrows
   :curry-compose-reader-macros
   :iterate
   :metabang-bind
   :split-sequence)
  (:export :smt :*solver*))
