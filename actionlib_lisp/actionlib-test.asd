;;;; -*- Mode: LISP -*-

(defsystem "actionlib-test"
  :depends-on ("lisp-unit" "actionlib-lisp" "roslisp" "actionlib-msg")
  :components
  ((:module "test"
            :components
            ((:file "package")
             (:file "state-machine-test" :depends-on ("package"))
             (:file "action-client-test" :depends-on ("package"))
             (:file "comm-state-machine-test" :depends-on ("package"))
             (:file "simple-comm-state-machine-test" :depends-on ("package"))
             (:file "goal-handle-test" :depends-on ("package" "comm-state-machine-test"))))))
             
  