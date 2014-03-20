;;;; -*- Mode: LISP -*-

(defsystem "actionlib"
  :depends-on ("roslisp" "roslisp-utils" "actionlib_msgs-msg")
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "state-machine" :depends-on ("package"))
             (:file "action-utils" :depends-on ("package"))
             (:file "comm-state-machine" :depends-on ("package" "state-machine"))
             (:file "client-goal-handle" :depends-on ("package" "comm-state-machine"))
             (:file "goal-manager" :depends-on ("package" "comm-state-machine" "client-goal-handle"))
             (:file "action-client" 
              :depends-on ("package" "comm-state-machine" "client-goal-handle" "action-utils"))))))