
(defpackage :cl-tqdm
  (:use :cl)
  (:export
   #:tqdm))

(in-package :cl-tqdm)

(defstruct (TqdmBar
	    (:conc-name tqdm-)
	    (:print-function
	     (lambda (tqdm stream depth)
	       (declare (ignore depth))
	       (format stream "Tqdm([~a/~a]~%:identifier :~a~%:description \"~a\"~%:creation-time ~a)~%"
		       (tqdm-count-idx tqdm)
		       (tqdm-total-count tqdm)
		       (tqdm-identifier tqdm)
		       (or (unless (equal "" (tqdm-description tqdm))
			     (tqdm-description tqdm))
			   "No Descriptions.")
		       (tqdm-creation-time tqdm))))
	    (:constructor
		tqdm (identifier
		      total-count
		      description
		      &aux (creation-time (get-universal-time)))))
  "Tqdm Structure that contains informations.

APIs read this structure and update, rendering progress-bar in terminals."
  (identifier :no-name :type symbol)
  (total-count 0 :type fixnum)
  (count-idx 0 :type fixnum)
  (call-timestamps nil :type list)
  (creation-time 0 :type fixnum)
  (description "" :type string))
  
(defmacro with-tqdm (out
		     identifier
		     total-size
		     description
		     &body
		       body)
  (declare (type symbol identifier)
	   (type fixnum total-size)
	   (type string description))
  `(let ((,out (tqdm ,identifier ,total-size ,description)))
     ,@body))

(defun update (tqdm count-incf &key (description ""))
  (declare (optimize (speed 3))
	   (type symbol identifier)
	   (type fixnum count-incf)
	   (type tqdmbar tqdm))
  (incf (tqdm-count-idx tqdm) count-incf)
  (setf (tqdm-description tqdm) description)
  (print-object tqdm t)
  nil)
