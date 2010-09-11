
(in-package :cl-tf)

(define-condition tf-connectivity-error (error)
  ((source-frame :initarg :source-frame)
   (target-frame :initarg :target-frame)))

(defclass transformer ()
  ((transforms :initform (make-hash-table :test 'equal)
               :reader transforms)))

(defgeneric can-transform (tf &key target-frame source-frame time))

(defgeneric lookup-transform (tf &key source-frame target-frame time))

(defgeneric set-transform (tf transform))

(defgeneric transform-pose (tf &key target-frame pose))

(defgeneric transform-point (tf &key target-frame point))

(defmethod can-transform ((tf transformer) &key target-frame source-frame time)
  (check-type target-frame string)
  (check-type source-frame string)
  (or (equal target-frame source-frame)
      (let ((target-root (get-transforms-to-root (transforms tf) target-frame time))
            (source-root (get-transforms-to-root (transforms tf) source-frame time)))
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
  (check-type target-frame string)
  (check-type source-frame string)
  (when (equal target-frame source-frame)
    (return-from lookup-transform
      (make-stamped-transform target-frame source-frame (ros-time)
                              (make-3d-vector 0 0 0)
                              (make-quaternion 0 0 0 1))))
  (let* ((down-transforms (get-transforms-to-root (transforms tf) target-frame time))
         (up-transforms (get-transforms-to-root (transforms tf) source-frame time)))
    (let ((result-tf (cond ((and down-transforms up-transforms)
                            (apply #'transform* (transform-inv (apply #'transform* down-transforms))
                                   up-transforms))
                           ((and (not down-transforms) up-transforms)
                            (apply #'transform* up-transforms))
                           ((and down-transforms (not up-transforms))
                            (transform-inv (apply #'transform* down-transforms))))))
      (unless result-tf
        (error 'tf-connectivity-error :source-frame source-frame :target-frame target-frame))
      (make-stamped-transform target-frame source-frame
                              (or time (stamp (car (last up-transforms))))
                              (translation result-tf)
                              (rotation result-tf)))))

(defmethod set-transform ((tf transformer) (transform stamped-transform))
  (with-slots (transforms) tf
    (let ((cache (gethash (child-frame-id transform) transforms)))
      (unless cache
        (setf cache (make-instance 'transform-cache))
        (setf (gethash (child-frame-id transform) transforms) cache))
      (cache-transform cache transform))))

(defmethod transform-pose ((tf transformer) &key target-frame pose time)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((transform (lookup-transform tf
                                     :target-frame target-frame
                                     :source-frame (frame-id pose)
                                     :time time)))
    (assert transform () "Transform from `~a' to `~a' not found."
            (frame-id pose) target-frame)
    (change-class (cl-transforms:transform-pose transform pose)
                  'pose-stamped
                  :frame-id target-frame
                  :stamp (stamp transform))))

(defmethod transform-point ((tf transformer) &key target-frame point time)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((transform (lookup-transform tf
                                     :target-frame target-frame
                                     :source-frame (frame-id point)
                                     :time time)))
    (assert transform () "Transform from `~a' to `~a' not found."
            (frame-id point) target-frame)
    (change-class (cl-transforms:transform-point transform point)
                  'point-stamped
                  :frame-id target-frame
                  :stamp (stamp transform))))

(defun get-transforms-to-root (transforms frame-id time &optional result)
  "Returns the list of transforms from `frame-id' up to the root of
  the tree."
  (let ((current-cache (gethash frame-id transforms)))
    (if current-cache
        (let ((current-tf (get-cached-transform (gethash frame-id transforms) time)))
          (get-transforms-to-root transforms (frame-id current-tf)
                                  time (cons current-tf result)))
        result)))
