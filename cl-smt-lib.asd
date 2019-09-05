(defsystem :cl-smt-lib
  :description
  "SMT object supporting SMT-LIB communication over input and output streams"
  :version "1.0.0"
  :author "Eric Schulte <eschulte@grammatech.com>"
  :licence "BSD-3-Clause"
  :depends-on (named-readtables)
  :components ((:file "cl-smt-lib")))
