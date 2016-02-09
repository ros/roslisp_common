;;;; -*- Mode: LISP -*-

(defsystem "cl-transforms-stamped"
  :depends-on (cl-transforms roslisp tf-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "utilities" :depends-on ("package"))
             (:file "datatypes" :depends-on ("package"))
             (:file "ros-messages" :depends-on ("package" "datatypes"))
             (:file "conditions" :depends-on ("package"))
             (:file "transform-interface" :depends-on ("package"))))))
