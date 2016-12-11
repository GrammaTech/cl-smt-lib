(in-package :cl-smt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros :include-utf8))

(defvar *solver* nil "Holds the current solver process.")

(defclass smt ()
  ((solver :initarg :solver :accessor solver :initform :z3 :type symbol
           :documentation "Indicates the solver, :z3, :cvc4 or :echo.")
   (proc :accessor proc
         :documentation "solver process")))

;;; From http://stackoverflow.com/questions/15988870/how-to-interact-with-a-process-input-output-in-sbcl-common-lisp
(defun program-stream (program &optional args)
  (let ((process (sb-ext:run-program program args
                                     :input :stream
                                     :output :stream
                                     :wait nil
                                     :search t)))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
                           (sb-ext:process-input process)))))

;; This works well, as in the following.
;; CL-SMT> (defparameter *stream* (program-stream "z3 -in -smt2"))
;; ; Evaluation aborted on #<SIMPLE-ERROR "Couldn't execute ~S: ~A" {1005AA2833}>.
;; CL-SMT> (defparameter *stream* (program-stream "z3" '("-in" "-smt2")))
;; *STREAM*
;; CL-SMT> (format *stream* "(set-logic SCHULTE)")
;; NIL
;; CL-SMT> (finish-output *stream*)
;; NIL
;; CL-SMT> (read-line *stream*)
;; "unsupported"
;; NIL
;; CL-SMT> (read-line *stream*)
;; "; ignoring unsupported logic SCHULTE line: 1 position: 1"
;; NIL
;;
;; Just have to start a threaded reader which continually tries to
;; read, and pushes read values into an output queue.

(defgeneric start (smt) (:documentation "Start SMT's process."))
(defmethod start ((obj smt))
  (with-slots (solver proc) obj
    (setf proc
          (ecase solver
            (:z3 (sb-ext:run-program "unbuffer" '("-p" "z3" "-in" "-smt2")
                                     :search t
                                     :output :stream
                                     :input :stream
                                     :wait nil))
            (:cvc4 (sb-ext:run-program "unbuffer" '("-p" "cvc4" "--lang=smt2")
                                       :search t
                                       :output :stream
                                       :input :stream
                                       :wait nil))))))

(defmethod initialize-instance :after ((obj smt) &key)
  (start obj))


;;; Solver interaction
(defvar solver-replacements
  '(("qf_bv" "QF_BV"))
  "Symbols to replace in input to the solver.")

(defun solver-replace (string)
  (reduce (lambda-bind (str (regex replacememnt))
            (regex-replace-all regex str replacememnt))
          solver-replacements
          :initial-value string))

(defgeneric >> (smt &rest forms) (:documentation "Pass forms to SMT."))
(defmethod >> ((obj smt) &rest forms)
  (mapc (lambda (form)
          (write-string
           (solver-replace (with-output-to-string (out)
                             (write form :case :downcase :stream out)))
           (sb-ext:process-input (proc obj)))
          (write-char #\Newline (sb-ext:process-input (proc obj)))
          (finish-output (sb-ext:process-input (proc obj))))
        forms))

(defgeneric << (smt) (:documentation "Read output from SMT"))
(defmethod << ((obj smt))
  ;; (close (sb-ext:process-input (proc obj))) ; NOTE: Close triggers output.
  (with-open-stream (out (sb-ext:process-output (proc obj)))
    (iter (for result = (read-line out nil))
          (while result)
          (collect result))))


;;; SMT functions
(defun set-option (option value)
  (>> *solver* `(set-option ,option ,value)))

(defun set-logic (logic)
  (>> *solver* `(set-logic ,logic)))
