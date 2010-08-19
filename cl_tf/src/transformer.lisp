
(in-package :cl-tf)

(defclass transformer ()
  ((transforms :initform (make-hash-table :test 'equal)
               :reader transforms)))

(defgeneric can-transform (tf &key target-frame source-frame time))

(defgeneric lookup-transform (tf &key target-frame source-frame time))

(defgeneric set-transform (tf transform))

(defgeneric transform-pose (tf &key target-frame pose))

(defgeneric transform-point (tf &key target-frame point))

(defmethod can-transform ((tf transformer) &key target-frame source-frame time)
  (declare (ignore time))
  (check-type target-frame string)
  (check-type source-frame string)
  (or (equal target-frame source-frame)
      (let ((target-root (get-transforms-along-path (transforms tf) target-frame))
            (source-root (get-transforms-along-path (transforms tf) source-frame)))
        (cond ((and target-root source-root)
               (equal (frame-id (car target-root))
                      (frame-id (car source-root))))
              ((and (not target-root) source-root)
               (equal target-frame
                      (frame-id (car source-root))))
              ((and target-root (not source-root))
               (equal (frame-id (car target-root))
                      source-frame))))))

(defmethod lookup-transform (tf &key target-frame source-frame time)
  (declare (ignore time))
  (check-type target-frame string)
  (check-type source-frame string)
  (let ((up-transforms (reverse (mapcar #'transform-inv
                                        (get-transforms-along-path
                                         (transforms tf)
                                         source-frame))))
        (down-transforms (get-transforms-along-path (transforms tf)
                                                    target-frame)))
    (apply #'transform* (append down-transforms up-transforms))))

(defmethod set-transform ((tf transformer) transform)
  (check-type transform stamped-transform)
  (with-slots (transforms) tf
    (setf (gethash (child-frame-id transform) transforms)
          transform)))

(defmethod transform-pose ((tf transformer) &key target-frame pose)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((transform (lookup-transform tf
                                     :source-frame target-frame
                                     :target-frame (frame-id pose))))
    (assert transform () "Transform from `~a' to `~a' not found."
            (frame-id pose) target-frame)
    (change-class (cl-transforms:transform-pose transform pose)
                  'pose-stamped :frame-id target-frame)))

(defmethod transform-point ((tf transformer) &key target-frame point)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((transform (lookup-transform tf
                                     :source-frame target-frame
                                     :target-frame (frame-id point))))
    (assert transform () "Transform from `~a' to `~a' not found."
            (frame-id point) target-frame)
    (change-class (cl-transforms:transform-point transform point)
                  'point-stamped :frame-id target-frame)))

(defun get-transforms-along-path (transforms frame-id &optional result)
  "Returns the list of transforms from `frame-id' up to the root of
  the tree."
  (let ((current-tf (gethash frame-id transforms)))
    (if current-tf
        (get-transforms-along-path transforms (frame-id current-tf)
                                   (cons current-tf result))
        result)))
