(in-package :cl-utils)

(defun make-adjustable-vector ()
  (make-array 0 :adjustable t :fill-pointer 0))