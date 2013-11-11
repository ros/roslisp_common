;;;; -*- Mode: LISP -*-

(defsystem "cl-tf2"
  :depends-on (actionlib tf2_msgs-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "buffer-client" :depends-on ("package"))))))
