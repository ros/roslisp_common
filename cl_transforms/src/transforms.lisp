(in-package :cl-transforms)

(defclass transform ()
  ((translation :initarg :translation :reader translation :type 3d-vector)
   (rotation :initarg :rotation :reader rotation :type quaternion))
  (:documentation "Represents a rigid affine transform of R^3, consisting of a rotation (represented as a normalized-quaternion) and translation (represented as a 3d-vector).  Object should be treated as immutable."))

(defun make-transform (translation rotation)
  (make-instance 'transform :translation translation :rotation rotation))

(defmethod print-object ((obj transform) strm)
  (print-unreadable-object (obj strm :type t)
    (with-slots (translation rotation) obj
      (format strm "~{~<~%   ~a~>~}" (list translation rotation)))))

(defmethod initialize-instance :after ((tr transform) &key (validate-args t))
  (when validate-args
    (assert (is-normalized (rotation tr)) nil
            "Rotation component ~a is not normalized" (rotation tr))))

(defun transform-inv (trans)
  (let ((q-inv (q-inv (rotation trans))))
    (make-transform (rotate q-inv (v-inv (translation trans)))
                    q-inv)))

(defun transform* (&rest transforms)
  "Compose transforms by first rotating and then adding up the points.
   Processes from right to left, i.e. the right-most transformation
   pair of transformations is applied first."
  (reduce (lambda (prev trans)
            (make-transform (v+ (translation trans) (rotate (rotation trans) (translation prev)))
                            (q* (rotation prev) (rotation trans))))
          (reverse transforms)))

(defun transform-point (trans p)
  (declare (type transform trans) (type 3d-vector p))
  (v+ (translation trans) (rotate (rotation trans) p)))

