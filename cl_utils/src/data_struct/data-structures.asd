;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :data-structures
  :components
  ((:file "priority-queue"))
  :depends-on ("extended-reals" "cl-utils"))
  