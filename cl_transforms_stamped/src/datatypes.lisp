
(in-package :cl-transforms-stamped)

(defclass stamped ()
  ((frame-id :initarg :frame-id :reader frame-id :type string)
   (stamp :initarg :stamp :reader stamp :type float)))

(defclass transform-stamped (transform stamped)
  ((child-frame-id :initarg :child-frame-id :reader child-frame-id :type string)))

(defclass pose-stamped (pose stamped) ())

(defclass point-stamped (3d-vector stamped) ())

(defmethod print-object ((tr transform-stamped) strm)
  (print-unreadable-object (tr strm :type t)
    (with-slots (frame-id child-frame-id stamp translation rotation)
        tr
      (format strm "~<~%   FRAME-ID: \"~a\", CHILD-FRAME-ID: \"~a\", STAMP: ~a~>~%~{   ~<~a~>~^~%~}"
              frame-id child-frame-id stamp (list translation rotation)))))

(defmethod print-object ((p pose-stamped) strm)
  (print-unreadable-object (p strm :type t)
    (with-slots (frame-id stamp origin orientation) p
      (format strm "~<~%   FRAME-ID: \"~a\", STAMP: ~a~>~%~{   ~<~a~>~^~%~}"
              frame-id stamp (list origin orientation)))))

(defmethod print-object ((p point-stamped) strm)
  (print-unreadable-object (p strm :type t)
    (with-slots (frame-id stamp x y z) p
      (format strm "~<~%   FRAME-ID: \"~a\" STAMP: ~a~>~%   ~<V: (~a ~a ~a)~>"
              frame-id stamp x y z))))

(defun make-pose-stamped (frame-id stamp translation rotation)
  (make-instance 'pose-stamped
                 :frame-id frame-id
                 :stamp stamp
                 :origin translation
                 :orientation rotation))

(defun transform->transform-stamped (frame-id child-frame-id stamp transform)
  (make-instance 'transform-stamped
                 :frame-id frame-id
                 :child-frame-id child-frame-id
                 :stamp stamp
                 :translation (translation transform)
                 :rotation (rotation transform)))

(defun make-point-stamped (frame-id stamp 3d-vector)
  (make-instance 'point-stamped
                 :frame-id frame-id
                 :stamp stamp
                 :x (x 3d-vector)
                 :y (y 3d-vector)
                 :z (z 3d-vector)))

(defun point->point-stamped (frame-id stamp point)
  (make-instance 'point-stamped
    :frame-id frame-id
    :stamp stamp
    :x (cl-transforms:x point)
    :y (cl-transforms:y point)
    :z (cl-transforms:z point)))

(defun make-transform-stamped (frame-id child-frame-id stamp translation rotation)
  (make-instance 'transform-stamped
                 :frame-id frame-id
                 :child-frame-id child-frame-id
                 :stamp stamp
                 :translation translation
                 :rotation rotation))

(defun copy-pose-stamped (pose &key origin orientation stamp)
  "Copies a pose-stamped. If either `origin' or `orientation' is
  specified, replaces the corresponding entry in the new pose."
  (with-slots (frame-id)
      pose
    (make-pose-stamped
     frame-id (or stamp (stamp pose))
     (or origin (origin pose))
     (or orientation (orientation pose)))))

(defun pose->pose-stamped (frame-id stamp pose)
  (make-instance 'pose-stamped
    :frame-id frame-id
    :stamp stamp
    :origin (origin pose)
    :orientation (orientation pose)))

(defun pose-stamped->pose (pose-stamped)
  (make-pose (origin pose-stamped) (orientation pose-stamped)))

(defun transform-stamped->pose-stamped (transform)
  (with-slots (child-frame-id stamp)
      transform
    (change-class (make-identity-pose) 'pose-stamped
                  :frame-id child-frame-id
                  :stamp stamp)))

(defgeneric ensure-pose-stamped (position-object &optional frame-id stamp)
  (:method ((pose-stamped pose-stamped) &optional frame-id stamp)
    (declare (ignore frame-id stamp))
    pose-stamped)
  (:method ((pose cl-transforms:pose) &optional frame-id stamp)
    (assert (and frame-id stamp) ()
            "Cannot convert POSE into POSE-STAMPED without `frame-id' and `stamp'.")
    (pose->pose-stamped frame-id stamp pose))
  (:method ((transform-stamped transform-stamped) &optional frame-id stamp)
    (make-pose-stamped
     (or frame-id (frame-id transform-stamped))
     (or stamp (stamp transform-stamped))
     (translation transform-stamped)
     (rotation transform-stamped))))


;;; Deprecated stamped-transform
(defclass stamped-transform (transform-stamped) ())
(defmethod initialize-instance :after ((instance stamped-transform) &key)
  (warn "STAMPED-TRANSFORM is deprecated. Use TRANSFORM-STAMPED instead."))
(defun make-stamped-transform (frame-id child-frame-id stamp translation rotation)
  (warn "MAKE-STAMPED-TRANSFORM is deprecated. Use MAKE-TRANSFORM-STAMPED instead.")
  (make-transform-stamped frame-id child-frame-id stamp translation rotation))
(defun transform->stamped-transform (frame-id child-frame-id stamp transform)
  (warn "TRANSFORM->STAMPED-TRANSFORM is deprecated.
Use TRANSFORM->TRANSFORM-STAMPED instead.")
  (transform->transform-stamped frame-id child-frame-id stamp transform))
(defun stamped-transform->pose-stamped (transform)
  (warn "STAMPED-TRANSFORM->POSE-STAMPED is deprecated.
Use TRANSFORM-STAMPED->POSE-STAMPED instead.")
  (transform-stamped->pose-stamped transform))
