;;;; -*- Mode: LISP -*-

(defsystem "cl-tf2"
  :depends-on (roslisp actionlib tf2_msgs-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "errors" :depends-on ("package"))
             (:file "buffer-interface" :depends-on ("package"))
             (:file "buffer-client" :depends-on ("package" "errors" "buffer-interface"))))))
