(defpackage :cl-smt-lib/process-two-way-stream
  (:use :cl :cl-smt-lib/fundamental-two-way-stream :uiop/launch-program)
  (:export :process-two-way-stream
           :make-process-two-way-stream))
(in-package :cl-smt-lib/process-two-way-stream)

;;; A process wrapped in a two-way stream.
(defclass process-two-way-stream (fundamental-two-way-stream)
  ((process :initarg :process :initform (error "process argument is required")
            :reader process))
  (:documentation
   "A fundamental-two-way-stream wrapping a single process' input and output."))

(defun make-process-two-way-stream (program &rest args)
  "Wrap PROCESS in an PROCESS-TWO-WAY-STREAM object."
  (let ((process (launch-program (format nil "~{~a~^ ~}" (cons program args))
                                 :input :stream
                                 :output :stream
                                 :wait nil
                                 :search t)))
    (make-instance 'process-two-way-stream
      :element-type '(unsigned-byte 8)
      :input (process-info-output process)
      :output (process-info-input process)
      :process process)))
