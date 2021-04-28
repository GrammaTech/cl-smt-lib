(defpackage :cl-smt-lib/fundamental-two-way-stream
  (:use :cl :trivial-gray-streams)
  (:export :fundamental-two-way-stream))
(in-package :cl-smt-lib/fundamental-two-way-stream)

(defclass fundamental-two-way-stream
    (fundamental-input-stream fundamental-output-stream)
  ((input :initarg :input :accessor input)
   (output :initarg :output :accessor output))
  (:documentation
   "A two-way stream composed of fundamental-{input,output}-streams."))

;;; Trivial-gray-stream generic function customization.
(defmethod stream-read-char ((stream fundamental-two-way-stream))
  (read-char (input stream)))

(defmethod stream-read-char-no-hang ((stream fundamental-two-way-stream))
  (read-char-no-hang (input stream)))

(defmethod stream-read-line ((stream fundamental-two-way-stream))
  (read-line (input stream)))

(defmethod stream-read-sequence ((stream fundamental-two-way-stream)
                                 sequence start end &key &allow-other-keys)
  (read-sequence sequence (input stream) :start start :end end))

(defmethod stream-unread-char ((stream fundamental-two-way-stream) character)
  (unread-char character (input stream)))

(defmethod stream-line-column ((stream fundamental-two-way-stream)) 0)

(defmethod stream-write-char ((stream fundamental-two-way-stream) character)
  (write-char character (output stream)))

(defmethod stream-write-sequence ((stream fundamental-two-way-stream)
                                  sequence start end &key &allow-other-keys)
  (write-sequence sequence (output stream) :start start :end end))

(defmethod stream-write-string
    ((stream fundamental-two-way-stream) string &optional (start 0) end)
  (write-string string (output stream) :start start :end end))

(defmethod stream-finish-output ((stream fundamental-two-way-stream))
  (finish-output (output stream)))
