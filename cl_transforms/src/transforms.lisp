(in-package :cl-transforms)

(defclass transform ()
  ((translation :initarg :translation :reader translation :type 3d-vector)
   (rotation :initarg :rotation :reader rotation :type quaternion))
  (:documentation "Represents a rigid affine transform of R^3, consisting of a rotation (represented as a normalized-quaternion) and translation (represented as a 3d-vector).  Object should be treated as immutable."))

(defmethod initialize-instance :after ((tr transform) &rest args
                                       &key (validate-args t))
  (when validate-args
    (assert (is-normalized (rotation tr)) nil
            "Rotation component ~a is not normalized" (rotation tr))))



(defun apply-transform (tr v)
  "Apply transform to a point (first rotate, then translate)"
  
  )




