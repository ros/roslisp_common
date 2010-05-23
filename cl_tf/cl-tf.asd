;;;; -*- Mode: LISP -*-

(defsystem "cl-tf"
  :depends-on (cl-transforms roslisp tf-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "messages" :depends-on ("package"))
             (:file "transformer" :depends-on ("package"))
             (:file "transform-listener"
                    :depends-on ("package" "messages" "transformer"))
             ;; (:file "transform-broadcaster"
             ;;        :depends-on ("package" "messages" "transformer"))
             ))))