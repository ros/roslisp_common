;;;; -*- Mode: LISP -*-

(defsystem "actionlib-test"
  :depends-on ("lisp-unit" "actionlib-lisp" "roslisp" "actionlib-msg"
                           "actionlib_tutorials-msg")
  :components
  ((:file "package")
   (:file "comm-state-machine-test" :depends-on ("package"))
   (:file "goal-handle-test" :depends-on ("package" "comm-state-machine-test"))))
             
  
