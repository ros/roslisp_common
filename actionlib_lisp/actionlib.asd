;;;; -*- Mode: LISP -*-

(defsystem "actionlib"
  :depends-on ("roslisp" "roslisp-utils" "actionlib_msgs-msg")
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "state-machine" :depends-on ("package"))
             (:file "action-client-stm" :depends-on ("package" "state-machine"))
             (:file "action-utils" :depends-on ("package"))
             (:file "action-client" 
              :depends-on ("package" "state-machine" "action-client-stm" "action-utils"))))))