;;;; -*- Mode: LISP -*-

(defsystem "roslisp-utilities"
  :depends-on ("roslisp")
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "ros-node" :depends-on ("package"))
             (:file "lispification" :depends-on ("package"))))))
