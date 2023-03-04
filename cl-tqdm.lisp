
(defpackage :cl-tqdm
  (:use :cl)
  (:export
   #:tqdm
   #:config
   #:with-config
   #:with-tqdm
   #:update))

(in-package :cl-tqdm)

(defstruct (TqdmBar
	    (:conc-name tqdm-)
	    (:print-function
	     (lambda (tqdm stream depth)
	       (declare (ignore depth))
	       (render-progress-bar stream tqdm)
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
		      &optional
			(description "")
		      &aux (creation-time (get-universal-time)))))
  "Tqdm Structure that contains informations.

APIs read this structure and update, rendering progress-bar in terminals.

Example:

```lisp
(print (tqdm :FirstBar 100))
```"
  (identifier :no-name :type symbol)
  (total-count 0 :type fixnum)
  (count-idx 0 :type fixnum)
  (call-timestamps nil :type list)
  (creation-time 0 :type (integer 0 4611686018427387903))
  (description "" :type string))

(defstruct (TqdmConfig
	    (:conc-name config-)
	    (:print-function
	     (lambda (config stream depth)
	       (declare (ignore depth))
	       (format stream "TqdmConfig{~%  :animation ~a ~%}"
		       (config-animation config))))
	    (:constructor
		config (&key
			  (animation t)
			  (space-string " "))))
  (animation t :type boolean)
  (space-string " " :type string))

(defvar *tqdm-config* (config
		       :animation t
		       :space-string " "))

(defmacro with-tqdm (out
		     identifier
		     total-size
		     description
		     &body
		       body)
  "Example:
(with-tqdm x :ProgressBar1 100 \"\"
  (update x))"
  (declare (type symbol identifier)
	   (type fixnum total-size)
	   (type string description))
  `(let ((,out (tqdm ,identifier ,total-size ,description)))
     ,@body))

(defmacro with-config (config &body body)
  "Example:
(with-config (config :animate nil)
  (with-tqdm
    ~~~))"
  (declare (type TqdmConfig config))
  `(let ((*tqdm-config* ,config))
     ,@body))

(defun update (tqdm count-incf &key (description "") (stream t))
  (declare (optimize (speed 3))
	   (type symbol identifier)
	   (type fixnum count-incf)
	   (type tqdmbar tqdm))
  (incf (tqdm-count-idx tqdm) count-incf)
  (setf (tqdm-description tqdm) description)
  (push (- (or
	    (the (or null (integer 0 4611686018427387903)) (car (last (tqdm-call-timestamps tqdm))))
	    (tqdm-creation-time tqdm))
	   (the (integer 0 4611686018427387903) (get-universal-time)))
	(tqdm-call-timestamps tqdm))				       
  (print-object tqdm stream)
  nil)

(defun render-progress-bar (stream tqdm)
  (declare (optimize (speed 3))
	   (type TqdmBar tqdm))

  )
