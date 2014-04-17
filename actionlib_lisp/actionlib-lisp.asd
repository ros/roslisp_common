;;;; -*- Mode: LISP -*-

(defsystem "actionlib-lisp"
  :depends-on ("roslisp" "roslisp-utilities" "actionlib_msgs-msg" "actionlib_tutorials-msg")
  :components
  ((:module "src"
            :components
            ((:file "package-devel")
             (:file "state-machine" :depends-on ("package-devel"))
             (:file "action-utils-devel" :depends-on ("package-devel"))
             (:file "comm-state-machine" :depends-on ("package-devel" "state-machine"))
             (:file "simple-comm-state-machine" :depends-on ("package-devel" "comm-state-machine"))
             (:file "client-goal-handle" :depends-on ("package-devel" "comm-state-machine"))
             (:file "goal-manager" :depends-on ("package-devel" "comm-state-machine" 
                                                                "client-goal-handle"
                                                                "simple-comm-state-machine"))
             (:file "action-client-devel" :depends-on ("package-devel" "comm-state-machine" 
                                                                       "simple-comm-state-machine"
                                                                       "client-goal-handle"
                                                                       "action-utils-devel"))
             (:file "simple-action-client" :depends-on ("package-devel" "action-client-devel"))))))
