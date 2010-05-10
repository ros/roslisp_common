;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem :cl-graph
  :components
  ((:file "package")
   (:file "graph" :depends-on ("package"))
   (:file "tree" :depends-on ("graph"))
   (:file "shortest-paths" :depends-on ("graph")))
  :depends-on ("cl-utils" "extended-reals" "data-structures"))