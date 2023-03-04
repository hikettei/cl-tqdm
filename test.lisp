
(in-package :cl-user)

(load "cl-tqdm.lisp")

(use-package :cl-tqdm)

(defpackage :cl-tqdm/test
  (:use :cl :cl-tqdm))

(with-tqdm x :bar1 10000 ""
  (dotimes (i 10000)
    (sleep 0.01)
    (update x)))
