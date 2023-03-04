
(load "cl-tqdm.asd")

(ql:quickload :cl-tqdm)

(defpackage :cl-tqdm/test
  (:use :cl :cl-tqdm))

(with-tqdm :bar1 100
  (dotimes (i 100)
    (update :bar1 1 "A")))
