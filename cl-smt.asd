(defsystem :cl-smt
  :description "Common Lisp interface to SMT Lib 2"
  :version "0.0.0"
  :author "Eric Schulte <eschulte@grammatech.com>"
  :licence "GPL V3"
  :components ((:file "package")
               (:file "smt" :depends-on ("package"))))
