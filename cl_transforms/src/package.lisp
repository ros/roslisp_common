(defpackage :cl-transforms
  (:use :cl :cl-utils)
  (:export :quaternion :quaternion-coefficient :gen-quaternion
           :x :y :z :w :make-quaternion :make-identity-rotation
           :copy-quaternion :q= :q* :q-inv :q+ :q- :q-norm :q-dot :q-scale
           :squared-norm :3d-vector :make-3d-vector :copy-3d-vector
           :make-identity-vector :v+ :v- :v* :v-inv :dot-product :normalize-vector
           :cross-product :v-dist :v-norm :rotate :axis-angle->quaternion
           :quaternion->axis-angle :yaw :get-yaw
           :euler->quaternion :matrix->quaternion :normalize
           :is-normalized :rotate :angle-between-quaternions
           :normalize-angle
           :transform :make-transform :make-identity-transform :copy-transform
           :transform-inv :transform* :transform-point :translation :rotation
           :point :pose :make-pose :make-identity-pose :copy-pose
           :make-2d-pose :transform-pose :reference-transform :origin :orientation
           :transform->pose :pose->transform
           :matrix->quaternion :quaternion->matrix :matrix->transform :transform->matrix 
           :pose->matrix :slerp :interpolate-vector :invert-rot-matrix :transpose-rot-matrix
           :transpose-2d-matrix :column-vectors->quaternion
           :twist :make-twist :make-identity-twist :copy-twist
           :wrench :make-wrench :make-identity-wrench :copy-wrench))