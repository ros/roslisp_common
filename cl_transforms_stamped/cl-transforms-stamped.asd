;;;; -*- Mode: LISP -*-

(defsystem "cl-transforms-stamped"
  :depends-on (cl-transforms roslisp tf-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "messages" :depends-on ("package"))
             (:file "conditions" :depends-on ("package"))
             (:file "transform-interface" :depends-on ("package"))))))
