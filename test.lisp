
(in-package :cl-user)

(load "cl-tqdm.lisp")

(use-package :cl-tqdm)

(defpackage :cl-tqdm/test
  (:use :cl :cl-tqdm))

(fresh-line)

(format t "Welcome to cl-tqdm!🔰~%cl-tqdm can be used like:~%Displaying Losses and progress during training.~%")

(with-tqdm x :bar1 10000 "Loss:0.0"
  (dotimes (i 10000)
    (sleep 0.001)
    (update x :description (format nil "Loss:~a.011" (random 10)))))

(format t "~%~%🗒In the near future, to line up several progress-bars will be implemented...~%")
(format t "~%🗒 More and documentations will be added!~%")
(format t "~%✅ Pull requests are welcome at original repository!~%")
