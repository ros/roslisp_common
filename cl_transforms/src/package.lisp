(defpackage :cl-transforms
  (:use :cl :cl-utils)
  (:export :quaternion :quaternion-coefficient :gen-quaternion
           :x :y :z :w :make-quaternion
           :q= :q* :q-inv :q+ :q- :q-norm :q-dot :q-scale :squared-norm
           :3d-vector :make-3d-vector :v+ :v- :v* :v-inv
           :dot-product :cross-product 
           :rotate :axis-angle->quaternion :quaternion->axis-angle :yaw
           :euler->quaternion :matrix->quaternion :normalize
           :is-normalized :rotate :angle-between-quaternions
           :transform :make-transform :transform-inv :transform* :transform-point :translation :rotation
           :pose :make-pose :make-2d-pose :transform-pose :reference-transform :origin :orientation
           :matrix->quaternion :matrix->transform :transform->matrix
           :slerp :interpolate-vector))

