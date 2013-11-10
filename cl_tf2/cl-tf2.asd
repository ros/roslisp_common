;;;; -*- Mode: LISP -*-

(defsystem "cl-tf2"
  :depends-on (actionlib)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "buffer-client" :depends-on ("package"))))))
