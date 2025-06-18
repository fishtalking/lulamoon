;; Bidirectional binary stream for Common Lisp
;; FIXME: Only works in SBCL

(uiop:define-package lulamoon.two-way-byte-stream
  (:export #:make
	   #:input-available-p)
  (:use #:cl))
(in-package :lulamoon.two-way-byte-stream)

(defclass two-way-byte-stream (sb-gray:fundamental-binary-stream)
  ((data :initform nil
	 :type (or (vector) null))
   (index :initform 0
	  :type integer))
  (:documentation
   "A stream that supports `write-byte', `write-sequence', `read-byte' and `read-sequence'"))

(defmethod stream-element-type ((stream two-way-byte-stream))
  `(unsigned-byte 8))

(defun make ()
  (make-instance 'two-way-byte-stream))


;; Reading

(declaim (inline input-available-p))
(defun input-available-p (stream)
  "Returns T if it can be read from STREAM without it throwing `end-of-file'"
  (with-slots (data index) stream
    (< index (length data))))

(defmethod sb-gray:stream-read-byte ((stream two-way-byte-stream))
  "Reads a byte from STREAM as an integer. `end-of-file' may be thrown."
  (with-slots (data index) stream
    (when (or (>= index (length data))
	      (null data))
      (error 'end-of-file :stream stream))
    (prog1
	(aref data index)
      (when (>= (incf index) (length data))
	(setf index 0)
	(setf data nil)))))

(defmethod sb-gray:stream-read-sequence ((stream two-way-byte-stream)
					 (seq sequence)
					 &optional start end)
  "Read and write from STREAM to SEQ. Ignores START and END. Returns bytes read"
  (with-slots (data) stream
    (loop with i = 0
	  while (and (< i (length seq))
		     (input-available-p stream))
	  do
	     (setf (aref seq i) (read-byte stream))
	     (incf i)
	  finally (return i))))


;; Writing

(defmethod sb-gray:stream-write-byte ((stream two-way-byte-stream) int)
  "Writes byte INT to STREAM."
  (with-slots (data index) stream
    (when (null data)
      (setf data (make-array 0 :element-type `(unsigned-byte 8) :fill-pointer 0)))
    (vector-push-extend int data)))

(defmethod sb-gray:stream-write-sequence ((stream two-way-byte-stream) seq
					  &optional start end)
  "Writes every byte in SEQ to STREAM. START and END are ignored."
  (with-slots (data index) stream
    (when (null data)
      ;; FIXME: could be faster by setting (data-of stream) directly to SEQ
      (setf data (make-array 0 :element-type `(unsigned-byte 8) :fill-pointer 0)))
    (loop for byte across seq
	  do (vector-push-extend byte data))))
