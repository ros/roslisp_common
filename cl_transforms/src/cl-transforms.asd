;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem "cl-transforms"
    :components ((:file "quaternions")
                 (:file "transform-package" :depends-on ("quaternions"))
                 (:file "quaternion-rotations"
                        :depends-on ("transform-package")))
    :depends-on ("cl-utils"))