;;;; -*- Mode: LISP -*-

(defsystem "actionlib-test-server"
  :depends-on ("roslisp" "roslisp-utilities" "actionlib_msgs-msg" "actionlib_tutorials-msg")
  :components
  ((:file "package")
   (:file "action-server" :depends-on ("package"))))