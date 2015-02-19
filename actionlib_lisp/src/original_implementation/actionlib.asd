;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem "actionlib"
  :components
  ((:file "package")
   (:file "action-utils" :depends-on ("package"))
   (:file "simple-action-server" :depends-on ("package" "action-utils"))
   (:file "action-client" :depends-on ("package" "action-utils")))
  :depends-on ("roslisp" "roslisp-utils" "actionlib_msgs-msg"
                         "cl-utils" "data-structures"))

