
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
	       (update tqdm :incf 0 :stream stream)
	       nil))
	    (:constructor
		tqdm (total-count
		      &optional
			(identifier :no-name)
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
	       (format stream "TqdmConfig{~% :animation ~a ~% :space-string ~a~% :bar-string ~a :indent ~a~%}"
		       (config-animation config)
		       (config-space-string config)
		       (config-bar-string config)
		       (config-indent config))))
	    (:constructor
		config (&key
			  (animation t)
			  (space-string " ")
			  (bar-string "█")
			  (indent 0))))
  (animation t :type boolean)
  (space-string " " :type string)
  (bar-string "█" :type string)
  (indent 0 :type fixnum))

(defparameter *tqdm-config* (config
			     :animation t
			     :space-string " "))

(defparameter *in-update-method* nil)

(defmacro with-tqdm (out
		     total-size
		     description
		     &body
		       body)
  "Example:
(with-tqdm x :ProgressBar1 100 \"\"
  (update x))"
  (declare (type fixnum total-size)
	   (type string description))
  `(let ((,out (tqdm ,total-size :with-tqdm ,description)))
     (fresh-line)
     ,@body))

(defmacro with-config (config &body body)
  "Example:
(with-config (config :animation nil)
  (with-tqdm
    ~~~))"
;  (declare (type TqdmConfig config))
  `(let ((*tqdm-config* ,config))
     ,@body))

(defmacro with-no-animation (&body body)
  `(with-config (config :animation nil)
     ,@body))

(defun update (tqdm &key (incf 1) (description "") (stream t))
  (declare ;(optimize (speed 3))
	   (type symbol identifier)
	   (type fixnum incf)
	   (type tqdmbar tqdm))
  (incf (tqdm-count-idx tqdm) incf)
  (setf (tqdm-description tqdm) description)
  (push (- (the
	    (integer 0 4611686018427387903)
	    (get-universal-time))
	   (tqdm-creation-time tqdm)
	      (or
	       (the (or null (integer 0 4611686018427387903)) (car (tqdm-call-timestamps tqdm)))
	       0))
	(tqdm-call-timestamps tqdm))
  (let ((*in-update-method* t))
    (render-progress-bar stream tqdm)
    nil))

(defun progress-percent (status)
  (fround (* 100 (/ (tqdm-count-idx status) (tqdm-total-count status)))))

(declaim (ftype (function (tqdmbar) string) render))
(defun render (status)
  "Rendering given status (which is the structure of tqdmbar), render returns the output string."
  (declare ;(optimize (speed 3))
	   (type tqdmbar status))
  (with-output-to-string (bar)
    (let ((spl (- (config-indent *tqdm-config*) (length (tqdm-description status)) -1)))
      (write-string (tqdm-description status) bar)
      (dotimes (_ spl) (write-string " " bar))
      (unless (equal (tqdm-description status) "")
	(write-string ":" bar)
	(write-string " " bar)))
    (let* ((n (the fixnum (round (the single-float (progress-percent status)))))
	   (r (round (if (>= (/ n 10) 10.0) 10 (/ n 10)))))
      (if (< n 100)
	  (write-string " " bar))
      (write-string (write-to-string n) bar)
      (write-string "% |" bar)
      (dotimes (_ r) (write-string (config-bar-string *tqdm-config*) bar))
      (dotimes (_ (- 10 r)) (write-string (config-space-string *tqdm-config*) bar)))
    (write-string "| " bar)
    (write-string (write-to-string (tqdm-count-idx status)) bar)
    (write-string "/" bar)
    (write-string (write-to-string (tqdm-total-count status)) bar)
    (write-string " [" bar)
    (let* ((now-time (get-universal-time))
	   (dif (- now-time (tqdm-creation-time status))))
      (write-string (write-to-string (coerce dif 'single-float)) bar)
      (write-string "s<" bar))
    (let* ((average-sec (coerce
			; (/ (apply #'+ (tqdm-call-timestamps status))
			;    (length (tqdm-call-timestamps status)))
			 (/ (+ (or
				(car (tqdm-call-timestamps status))
				0.0)
			       (or
				(second (tqdm-call-timestamps status))
				0.0))
			    (if (second (tqdm-call-timestamps status))
				2.0
				1.0))
			 'single-float))
	   (total (tqdm-total-count status))
	   (ts (tqdm-call-timestamps status))
	   (average-sec (/ average-sec (if (= 0.0 (car ts))
					   1.0
					   (car ts)))))
      (setf (tqdm-call-timestamps status) `(,0
					    ,average-sec)) ;decay rate?
      (write-string (write-to-string (* average-sec total)) bar)
      (write-string "s, " bar)
      (write-string (write-to-string average-sec) bar)
      (write-string "s/it]" bar))))

(defun backward-lines ()
  (write-char #\Return)
  (write-char #\Rubout))

(defun render-progress-bar (stream tqdm)
  (declare ;(optimize (speed 3))
	   (type TqdmBar tqdm))

  (if (and
       (config-animation *tqdm-config*)
       *in-update-method*)
      ; delete the current progress-bar
      (backward-lines)
      (fresh-line))
  (format stream (render tqdm))
  nil)
