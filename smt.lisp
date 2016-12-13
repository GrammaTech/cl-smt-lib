(in-package :cl-smt)

;;; From http://stackoverflow.com/questions/15988870/~
;;;             how-to-interact-with-a-process-input-output-in-sbcl-common-lisp
(defun program-stream (program &optional args)
  (let ((process #+sbcl (sb-ext:run-program program args
                                            :input :stream
                                            :output :stream
                                            :wait nil
                                            :search t)
                 #-sbcl (error "CL-SMT currently only supports SBCL.")))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
                           (sb-ext:process-input process)))))

(defvar *previous-readtables* nil
  "Holds *readtable* before cl-smt enabled the #! case preserving reader.")

(defun read-preserving-case (stream char n)
  (declare (ignorable char) (ignorable n))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t nil t)))

(defmacro enable-preserving-case-syntax ()
  "Register the #! reader macro to read the next form preserving case."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (set-dispatch-macro-character #\# #\! #'read-preserving-case)))

(defmacro disable-preserving-case-syntax ()
  "Un-register the #! reader macro restoring the previous *readtable*."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (pop *previous-readtables*))))

(defvar *smt-debug-stream* nil "Optional stream to echo all SMT input.")

(defun smt (stream forms)
  "Print FORMS to STREAM preserving case.
SMT is suitable to print case-sensitive forms in smtlib2 format."
  ;; Setting the `readtable-case' to :PRESERVE ensures `format'
  ;; doesn't print pipes around variable names.
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (mapc (lambda (form) (format stream "~S~%~%" form)) forms)
    (when *smt-debug-stream*
      (mapc (lambda (form) (format *smt-debug-stream* "~S~%~%" form)) forms))
    (finish-output stream)))
