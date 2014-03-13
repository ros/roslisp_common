;;;; -*- Mode: LISP -*-

(defsystem "actionlib"
  :depends-on ("roslisp" "roslisp-utils")
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "state-machine" :depends-on ("package"))
             (:file "action-client-stm" :depends-on ("package" "state-machine"))
             (:file "action-client" :depends-on ("package" "state-machine" "action-client-stm"))))))