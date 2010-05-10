;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :cl-utils
  :components
  ((:file "package")
   (:file "macros" :depends-on ("package"))
   (:file "functional" :depends-on ("macros"))
   (:file "seq-utils" :depends-on ("macros"))
   (:file "list-utils" :depends-on ("macros"))
   (:file "iteration" :depends-on ("macros"))
   (:file "array-utils" :depends-on ("macros"))
   (:file "hash-utils" :depends-on ("package"))
   (:file "math-utils" :depends-on ("package"))
   (:file "print-utils" :depends-on ("macros"))))


