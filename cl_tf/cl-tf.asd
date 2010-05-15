;;;; -*- Mode: LISP -*-

(defsystem "cl-tf"
  :depends-on (cl-transforms roslisp tf-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "messages" :depends-on ("package"))
             (:file "transformer" :depends-on ("package"))
             ;; (:file "transform-listener"
             ;;        :depends-on ("messages" "transformer"))
             ;; (:file "transform-broadcaster"
             ;;        :depends-on ("messages" "transformer"))
             ))))