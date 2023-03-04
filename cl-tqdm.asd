(in-package #:cl-user)

(asdf:defsystem :cl-cram
  :name "cl-tqdm"
  :description "Simple And Fast Progress Bar Library for Common Lisp"
  :author "hikettei"
  :license "MIT"
  :source-control (:git "git@github.com:hikettei/cl-cram.git")
  :serial t
  :components ((:file "cl-tqdm")))

