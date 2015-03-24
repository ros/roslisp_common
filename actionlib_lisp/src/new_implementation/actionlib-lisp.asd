;;;; -*- Mode: LISP -*-

(defsystem "actionlib-lisp"
  :depends-on ("roslisp" "roslisp-utilities" "cl-utils" "actionlib_msgs-msg")
  :components
  ((:file "package")
   (:file "state-machine" :depends-on ("package"))
   (:file "action-utils" :depends-on ("package"))
   (:file "comm-state-machine" :depends-on ("package" "state-machine"))
   (:file "client-goal-handle" :depends-on ("package" "comm-state-machine"))
   (:file "goal-manager" :depends-on ("package" "comm-state-machine" 
                                                "client-goal-handle"))
   (:file "action-client" :depends-on ("package" "comm-state-machine"
                                                 "client-goal-handle"
                                                 "action-utils"))
   (:file "simple-action-client" :depends-on ("package" "action-client"))))
