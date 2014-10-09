;;;; -*- Mode: LISP -*-

(in-package :asdf)

(defsystem "cl-transforms"
  :components ((:file "package")
               (:file "quaternions" :depends-on ("package"))
               (:file "3d-vector" :depends-on ("package"))
               (:file "quaternion-rotations" :depends-on ("package" "quaternions" "3d-vector"))
               (:file "interpolation" :depends-on ("package" "quaternions" "3d-vector"))
               (:file "transforms" :depends-on ("package"
                                                "quaternions"
                                                "quaternion-rotations"
                                                "3d-vector"))
               (:file "pose" :depends-on ("package" "transforms" "3d-vector"))
               (:file "matrix-conversions" :depends-on ("package"
                                                        "quaternions"
                                                        "quaternion-rotations"
                                                        "3d-vector"
                                                        "transforms"))
               (:file "screws" :depends-on ("package" "3d-vector")))
  :depends-on ("cl-utils"))
