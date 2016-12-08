(in-package :cl-smt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros :include-utf8))

(defvar *solver* :z3
  "SMT solver used by `cl-smt'.
Should be one of :z3 or :cvc4.")

(defun smt (&rest forms)
  (let ((proc (sb-ext:run-program "z3" '("-in" "-smt2")
                                  :search t
                                  :output :stream
                                  :input :stream
                                  :wait nil)))
    (with-open-stream (stream-var (sb-ext:process-input proc))
      (mapcar {write _ stream-var} forms))
    (with-open-stream (out-stream (sb-ext:process-output proc))
      (loop :for value = (read (sb-ext:process-output proc) nil :eof)
         :until (eql byte :eof)
         :collect value))))
