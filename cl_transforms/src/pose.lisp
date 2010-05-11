(in-package :cl-transforms)

(defclass pose ()
  ((origin :type 3d-vector :reader origin :initarg :origin)
   (orientation :type quaternion :reader orientation :initarg :orientation))
  (:documentation "Represents a 6 dof pose, consisting of an origin in R^3 and an orientation, represented as a quaternion"))

(defun make-pose (origin orientation)
  (make-instance 'pose :origin origin :orientation orientation))

(defun reference-transform (pose)
  "Return the transform that takes in the coordinates of a point in the pose's frame, and returns the coordinates in the reference frame"
  (make-transform (origin pose) (orientation pose)))

(defun transformed-identity (tr)
  "return the result of transforming the identity pose by a transform"
  (make-pose (translation tr) (rotation tr)))

(defun transform-pose (tr p)
  (transformed-identity (transform* tr (reference-transform p))))