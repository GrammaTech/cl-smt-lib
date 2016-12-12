(in-package :cl-smt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros :include-utf8))

;;; From http://stackoverflow.com/questions/15988870/~
;;;             how-to-interact-with-a-process-input-output-in-sbcl-common-lisp
(defun program-stream (program &optional args)
  (let ((process (sb-ext:run-program program args
                                     :input :stream
                                     :output :stream
                                     :wait nil
                                     :search t)))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
                           (sb-ext:process-input process)))))

(defvar *previous-readtables* nil)

(defun read-preserving-case (stream char n)
  (declare (ignorable char) (ignorable n))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t nil t)))

(defmacro enable-preserving-case-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (set-dispatch-macro-character #\# #\! #'read-preserving-case)))

(defmacro disable-preserving-case-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (pop *previous-readtables*))))

(defun smt (stream forms)
  ;; Setting the `readtable-case' to :PRESERVE ensures `format'
  ;; doesn't print pipes around variable names.
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (mapc {format stream "~S~%~%"} forms)
    (finish-output stream)))
