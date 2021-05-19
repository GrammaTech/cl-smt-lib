;;; cl-smt-lib.lisp --- Common Lisp SMT-Lib Integration
(defpackage :cl-smt-lib/cl-smt-lib
  (:nicknames :cl-smt-lib)
  (:use :common-lisp :named-readtables :cl-smt-lib/process-two-way-stream)
  (:import-from :uiop/launch-program :terminate-process :wait-process)
  (:export
   :make-smt
   :smt-error
   :ignore-smt-error
   :return-smt-error
   :write-to-smt
   :read-from-smt
   :with-smt
   :*smt-debug*
   ;; smt accessors
   :smt-output-stream
   :smt-input-stream
   :smt-process))
(in-package :cl-smt-lib/cl-smt-lib)
#+debug (declaim (optimize (debug 3)))

(defvar *smt-debug* nil
  "Set to a stream to duplicate smt input and output to the *SMT-DEBUG*.")

;;; Implementation depends on if two-way-stream is a class or structure.

(defclass smt (process-two-way-stream) ()
  (:documentation "An SMT process with input and output streams."))

(defun make-smt (program &rest args)
  "Wrap PROCESS in an SMT object."
  (apply #'make-process-two-way-stream program args))

(define-condition smt-error (error)
  ((text :initarg :text :initform nil :reader text)
   (smt :initarg :smt :initform nil :reader smt))
  (:report (lambda (condition stream)
             (format stream "SMT: ~a~%~S"
                     (text condition) (smt condition)))))

(defun write-to-smt (smt forms)
  "Write FORMS to the process in SMT over it's STDIN.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid
case-sensitive smt libv2 format."
  (let ((*readtable* (copy-readtable nil))
        (format-string "~{~S~^~%~}~%"))
    (setf (readtable-case *readtable*) :preserve)
    (format smt format-string forms)
    (when *smt-debug*
      (format *smt-debug* "~&;; WRITE-TO-SMT~%")
      (format *smt-debug* format-string forms)
      (finish-output *smt-debug*))
    (finish-output smt)))

(defun read-from-smt (smt &optional preserve-case-p (eof-error-p t) eof-value)
  "Write FORMS to the process in SMT over it's STDIN.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid
case-sensitive smt libv2 format."
  (let ((*readtable* (copy-readtable nil)))
    (when preserve-case-p
      (setf (readtable-case *readtable*) :preserve))
    (let ((value (read smt eof-error-p eof-value)))
      (when *smt-debug*
        (format *smt-debug* "~&;; READ-FROM-SMT~%")
        (write value :stream *smt-debug*)
        (finish-output *smt-debug*))
      (restart-case
          (if (and (listp value)
                   (equal (if preserve-case-p '|error| 'ERROR) (car value)))
              (error (make-condition 'smt-error
                       :text (second value)
                       :smt smt))
              value)
        (ignore-smt-error () :report "Ignore SMT error." nil)
        (return-smt-error () :report "Return SMT error." value)))))

(defmacro with-smt ((smt (program &rest args) &optional preserve-case-p)
                    &body body)
  (declare (ignore preserve-case-p))
  (let ((form (gensym))
        (status (gensym)))
    `(with-open-stream (,smt (make-smt ,program ,@args))
       (unwind-protect
            (progn
              ,@body
              (close (output ,smt))
              (let ((,status (wait-process (process ,smt))))
                (unless (zerop ,status) (error "SMT solver failed with exit status ~S" ,status)))
              (loop :for ,form = (read ,smt nil :eof)
                 :while (not (equal :eof ,form))
                 :collect ,form))
         ;; Ensure the process is terminated.
         (terminate-process (process ,smt))))))

(defun read-preserving-case (stream char n)
  (declare (ignorable char) (ignorable n))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t nil t)))

(unless (find-readtable :cl-smt-lib)
  (defreadtable :cl-smt-lib
    (:merge :current)
    (:dispatch-macro-char #\# #\! #'read-preserving-case)))
