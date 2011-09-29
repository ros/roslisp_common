;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :cl-probability
  :components
  ((:file "package")
   (:file "events" :depends-on ("package"))
   (:file "ops" :depends-on ("package"))
   (:file "alist" :depends-on ("ops" "events"))
   (:file "exp" :depends-on ("ops" "events"))
   (:file "vector" :depends-on ("ops" "events"))
   (:file "ctmc" :depends-on ("alist")))
  :depends-on ("cl-utils" "extended-reals"))