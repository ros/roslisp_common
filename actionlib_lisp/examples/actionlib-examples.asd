;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem "actionlib-examples"
  :components
  ((:file "pkg")
   (:file "fibonacci-server" :depends-on ("pkg"))
   (:file "fibonacci-client" :depends-on ("pkg")))
  :depends-on ("roslisp" "actionlib" "actionlib_tutorials-msg"))
