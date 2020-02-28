;;; cl-smt-lib.lisp --- Common Lisp SMT-Lib Integration
(defpackage :cl-smt-lib
  (:use :common-lisp :named-readtables)
  (:import-from :trivial-gray-streams
                :fundamental-input-stream
                :fundamental-output-stream
                :trivial-gray-stream-mixin)
  (:import-from :uiop/launch-program
                :launch-program
                :terminate-process
                :process-info-input
                :process-info-output)
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
(in-package :cl-smt-lib)
#+debug (declaim (optimize (debug 3)))

(defvar *smt-debug* nil
  "Set to a stream to duplicate smt input and output to the *SMT-DEBUG*.")

;;; Implementation depends on if two-way-stream is a class or structure.

(defclass smt (fundamental-input-stream
               fundamental-output-stream
               trivial-gray-stream-mixin)
  ((input :initarg :input :accessor smt-input-stream)
   (output :initarg :output :accessor smt-output-stream)
   (process :initarg :process :initform (error "process argument is required")
            :reader smt-process)))

(defun make-smt (program &rest args)
  "Wrap PROCESS in an SMT object."
  (let ((process (launch-program (format nil "~{~a~^ ~}" (cons program args))
                                 :input :stream
                                 :output :stream
                                 :wait nil
                                 :search t)))
    (make-instance 'smt
      :input (process-info-output process)
      :output (process-info-input process)
      :process process)))

;;; Trivial-gray-stream definitions. TODO: hanging.
(defmethod trivial-gray-streams:stream-read-char ((smt smt))
  (read-char (smt-input-stream smt)))
(defmethod trivial-gray-streams:stream-read-line ((smt smt))
  (read-line (smt-input-stream smt)))
(defmethod trivial-gray-streams:stream-read-sequence
    ((smt smt) sequence start end &key &allow-other-keys)
  (read-sequence sequence (smt-input-stream smt) :start start :end end))
(defmethod trivial-gray-streams:stream-unread-char ((smt smt) character)
  (unread-char character (smt-input-stream smt)))
(defmethod trivial-gray-streams:stream-line-column ((smt smt)) 0)
(defmethod trivial-gray-streams:stream-write-char ((smt smt) character)
  (write-char character (smt-output-stream smt)))
(defmethod trivial-gray-streams:stream-write-sequence
    ((smt smt) sequence start end &key &allow-other-keys)
  (write-sequence sequence (smt-output-stream smt) :start start :end end))
(defmethod trivial-gray-streams:stream-write-string
    (stream string &optional (start 0) end)
  (write-string string (smt-output-stream smt) :start start :end end))

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
  (let ((form (gensym)))
    `(with-open-stream (,smt (make-smt ,program ,@args))
       (unwind-protect
            (progn
              ,@body
              (close (smt-output-stream ,smt))
              (loop :for ,form = (read-from-smt ,smt ,preserve-case-p nil :eof)
                 :while (not (equal :eof ,form))
                 :collect ,form))
         ;; Ensure the process is terminated.
         (terminate-process (smt-process ,smt))))))

(defun read-preserving-case (stream char n)
  (declare (ignorable char) (ignorable n))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t nil t)))

(unless (find-readtable :cl-smt-lib)
  (defreadtable :cl-smt-lib
    (:merge :current)
    (:dispatch-macro-char #\# #\! #'read-preserving-case)))
