(in-package #:cl-user)

(asdf:defsystem :cl-tqdm
  :name "cl-tqdm"
  :description "Simple And Fast Progress Bar Library for Common Lisp"
  :author "hikettei"
  :version "v1.0"
  :license "MIT"
  :source-control (:git "git@github.com:hikettei/cl-tqdm.git")
  :serial t
  :components ((:file "cl-tqdm")))

