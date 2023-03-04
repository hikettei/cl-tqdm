
(defpackage :cl-tqdm
  (:use :cl)
  (:export))

(in-package :cl-tqdm)

(defvar *currently-processing-bar* nil)
(declaim (type list *currently-processing-bar*))

(defstruct (TqdmBar
	    (:conc-name tqdm-)
	    (:print-function
	     (lambda (tqdm stream depth)
	       (declare (ignore depth))
	       (format stream "tqdm(~a/~a :identifier :~a :description \"~a\")~%"
		       (tqdm-count-idx tqdm)
		       (tqdm-total-count tqdm)
		       (tqdm-identifier tqdm)
		       (or (unless (equal "" (tqdm-description tqdm))
			     (tqdm-description tqdm))
			   "No Descriptions."))))
	    (:constructor
		tqdm (identifier total-count description)))
  "Tqdm Structure that contains informations.

APIs read this structure and update, rendering progress-bar in terminals."
  (identifier :no-name :type symbol)
  (total-count 0 :type fixnum)
  (count-idx 0 :type fixnum)
  (description "" :type string))
  

(defmacro with-tqdm (identifier
		     total-size
		     &body
		       body)
  (declare (type symbol identifier)
	   (type fixnum total-size))
  `(let ((*currently-processing-bar* *currently-processing-bar*))
     (push (tqdm ) *currently-processing-bar*)))
