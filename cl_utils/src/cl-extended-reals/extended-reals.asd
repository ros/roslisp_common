;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :extended-reals
  :components
  ((:file "package")
   (:file "extended-reals" :depends-on ("package"))))