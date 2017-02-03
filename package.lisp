(defpackage :cl-smt-lib
  (:use :common-lisp)
  (:export
   :make-smt
   :smt-error
   :ignore-smt-error
   :return-smt-error
   :write-to-smt
   :read-from-smt
   :with-smt
   :enable-preserving-case-syntax
   :disable-preserving-case-syntax
   ;; smt accessors
   :smt-output-stream
   :smt-input-stream
   :smt-process))
