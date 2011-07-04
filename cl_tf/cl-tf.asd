;;;; -*- Mode: LISP -*-

(defsystem "cl-tf"
  :depends-on (cl-transforms roslisp tf-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "messages" :depends-on ("package"))
             (:file "algorithms" :depends-on ("package"))
             (:file "transform-cache" :depends-on ("package" "algorithms"))
             (:file "transformer" :depends-on ("package" "transform-cache" "messages"))
             (:file "transform-listener"
                    :depends-on ("package" "messages" "transformer"))
             (:file "broadcaster" :depends-on ("package" "messages"))))))
