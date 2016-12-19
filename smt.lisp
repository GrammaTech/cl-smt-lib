(in-package :cl-smt)

(defstruct (smt (:include two-way-stream)
             (:constructor %make-smt (input-stream output-stream process))
             (:copier nil)
             (:predicate nil))
  (process (sb-impl::missing-arg) :read-only t))

#+sbcl (sb-impl::defprinter (smt) process input-stream output-stream)

(defun make-smt (program &optional args)
  "Wrap PROCESS in an SMT object"
  (let ((process #+sbcl (sb-ext:run-program program args
                                            :input :stream
                                            :output :stream
                                            :wait nil
                                            :search t)
                 #-sbcl (error "CL-SMT currently only supports SBCL.")))
    (%make-smt (sb-ext:process-output process)
               (sb-ext:process-input process)
               process)))

(defun write-to-smt (smt forms)
  "Write FORMS to the process in SMT over it's STDIN.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid
case-sensitive smt libv2 format."
  (let ((*readtable* (copy-readtable nil))
        (format-string "~{~S~^~%~}~%"))
    (setf (readtable-case *readtable*) :preserve)
    (format smt format-string forms)
    (finish-output smt)))

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
