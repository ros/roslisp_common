;;;; -*- Mode: LISP -*-

(defsystem "actionlib-server"
  :depends-on ("roslisp" "roslisp-utilities" "actionlib_msgs-msg" "actionlib_tutorials-msg")
  :components
  ((:module "test-server"
            :components
            ((:file "package")
             (:file "action-server" :depends-on ("package"))))))