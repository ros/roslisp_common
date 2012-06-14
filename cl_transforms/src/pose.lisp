(in-package :cl-transforms)

(defclass pose ()
  ((origin :type point :reader origin :initarg :origin)
   (orientation :type gen-quaternion :reader orientation :initarg :orientation))
  (:documentation "Represents a 6 dof pose, consisting of an origin in R^3 and an orientation, represented as a normalized quaternion"))

(defun make-pose (origin orientation)
  (make-instance 'pose :origin origin :orientation orientation))

(defun make-identity-pose ()
  (make-pose
   (make-identity-vector)
   (make-identity-rotation)))

(defun copy-pose (pose &key origin orientation)
  (with-slots ((old-origin origin)
               (old-orientation orientation)) pose
    (make-pose
     (or origin (copy-3d-vector old-origin))
     (or orientation (copy-quaternion old-orientation)))))

(defmethod initialize-instance :after ((p pose) &key (validate-args :warn))
  (when validate-args
    (with-slots (orientation) p
      (unless (is-normalized orientation)
        (if (eq validate-args :warn)
            (progn
              (setq orientation (normalize orientation))
              (warn "Normalized orientation component to ~a" orientation))
            (error "Orientation component ~a not normalized" orientation))))))

(defmethod print-object ((obj pose) strm)
  (print-unreadable-object (obj strm :type t)
    (with-slots (origin orientation) obj
      (format strm "~{~<~%   ~a~>~}" (list origin orientation)))))

(defun make-2d-pose (x y theta)
  (make-pose (make-3d-vector x y 0.0)
             (axis-angle->quaternion (make-3d-vector 0 0 1) theta)))

(defgeneric reference-transform (pose)
  (:documentation "Return the transform that takes in the coordinates
  of a point in the pose's frame, and returns the coordinates in the
  reference frame")
  (:method ((pose pose))
    (make-transform (origin pose) (orientation pose)))
  (:method ((transform transform))
    transform))

(defun transform->pose (transform)
  (declare (type transform transform))
  (make-pose (translation transform) (rotation transform)))

(defun pose->transform (pose)
  (declare (type pose pose))
  (reference-transform pose))

(defun transformed-identity (tr)
  "return the result of transforming the identity pose by a transform"
  (make-pose (translation tr) (rotation tr)))

(defun transform-pose (tr p)
  (transformed-identity (transform* tr (reference-transform p))))


(defmethod transform ((tr transform) (p pose))
  (transform-pose tr p))