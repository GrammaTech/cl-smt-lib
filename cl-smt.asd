(defsystem :cl-smt
  :description "Common Lisp interface to SMT Lib 2"
  :version "0.0.0"
  :author "Eric Schulte <eschulte@grammatech.com>"
  :licence "GPL V3"
  :depends-on (alexandria
               cl-arrows
               curry-compose-reader-macros
               iterate
               metabang-bind
               split-sequence
               cl-ppcre)
  :components ((:file "package")
               (:file "smt" :depends-on ("package"))))
