;;;; -*- Mode: LISP -*-

(defsystem "roslisp-utils"
  :depends-on ("sbcl" "roslisp")
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "ros-node" :depends-on ("package"))
             (:file "lispification" :depends-on ("package"))))))
