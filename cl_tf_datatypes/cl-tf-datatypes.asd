;;;; -*- Mode: LISP -*-

(defsystem "cl-tf-datatypes"
  :depends-on (cl-transforms roslisp tf-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "messages" :depends-on ("package"))))))
