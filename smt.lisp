(in-package :cl-smt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros :include-utf8))

(defvar *solver* :cvc4 "Default solver used by `cl-smt' (one of :z3 or :cvc4).")

(defclass smt ()
  ((solver :initarg :solver :accessor solver :initform *solver* :type symbol)
   (proc :accessor proc)))

(defgeneric start (smt) (:documentation "Start SMT's process."))
(defmethod start ((obj smt))
  (with-slots (proc) obj
    (setf proc
          (ecase *solver*
            (:z3 (sb-ext:run-program "z3" '("-in" "-smt2")
                                     :search t
                                     :output :stream
                                     :input :stream
                                     :wait nil))
            (:cvc4 (sb-ext:run-program "cvc4" '("--lang=smt2")
                                       :search t
                                       :output :stream
                                       :input :stream
                                       :wait nil))))))

(defmethod initialize-instance :after ((obj smt) &key)
  (start obj))

(defgeneric >> (smt &rest forms) (:documentation "Pass forms to SMT."))
(defmethod >> ((obj smt) &rest forms)
  (mapc (lambda (form)
          (write form :case :downcase :stream (sb-ext:process-input (proc obj)))
          (write-char #\Newline (sb-ext:process-input (proc obj))))
        forms))

(defgeneric << (smt) (:documentation "Read output from SMT"))
(defmethod << ((obj smt))
  (close (sb-ext:process-input (proc obj))) ; NOTE: Close triggers output.
  (with-open-stream (out (sb-ext:process-output (proc obj)))
    (iter (for result = (read-line out nil))
          (while result)
          (collect result))))
