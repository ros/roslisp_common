
(in-package :cl-tf)

(define-condition tf-connectivity-error (error)
  ((source-frame :initarg :source-frame :reader source-frame)
   (target-frame :initarg :target-frame :reader target-frame)))

(define-condition tf-lookup-error (error)
  ((frame :initarg :frame :reader frame)))

(defclass transformer ()
  ((transforms :initform (make-hash-table :test 'equal)
               :reader transforms)
   (set-transform-callbacks :initform nil)
   (lock :initform (sb-thread:make-mutex))))

(defgeneric can-transform (tf &key target-frame source-frame time))

(defgeneric lookup-transform (tf &key target-frame source-frame time))

(defgeneric wait-for-transform (tf &key target-frame source-frame time))

(defgeneric set-transform (tf transform &key suppress-callbacks))

(defgeneric transform-pose (tf &key target-frame pose))

(defgeneric transform-point (tf &key target-frame point))

(defmethod can-transform ((tf transformer) &key target-frame source-frame time)
  (check-type target-frame string)
  (check-type source-frame string)
  (sb-thread:with-mutex ((slot-value tf 'lock))
    (handler-case
        (let ((target-frame (ensure-fully-qualified-name target-frame))
              (source-frame (ensure-fully-qualified-name source-frame))
              (time (ensure-null-time time)))
          (or (equal target-frame source-frame)
              (let ((target-root (get-transforms-to-root (transforms tf) target-frame time))
                    (source-root (get-transforms-to-root (transforms tf) source-frame time)))
                (check-transform-exists tf target-frame)
                (check-transform-exists tf source-frame)
                (cond ((and target-root source-root)
                       (equal (frame-id (car target-root))
                              (frame-id (car source-root))))
                      ((and (not target-root) source-root)
                       (equal target-frame
                              (frame-id (car source-root))))
                      ((and target-root (not source-root))
                       (equal (frame-id (car target-root))
                              source-frame))))))
      (tf-cache-error ()
        nil)
      (tf-lookup-error ()
        nil))))

(defmethod lookup-transform ((tf transformer) &key target-frame source-frame time)
  (check-type target-frame string)
  (check-type source-frame string)
  (let ((target-frame (ensure-fully-qualified-name target-frame))
        (source-frame (ensure-fully-qualified-name source-frame))
        (time (ensure-null-time time)))
    (when (equal target-frame source-frame)
      (return-from lookup-transform
        (make-stamped-transform target-frame source-frame (ros-time)
                                (make-3d-vector 0 0 0)
                                (make-quaternion 0 0 0 1))))
    (check-transform-exists tf target-frame)
    (check-transform-exists tf source-frame)
    (sb-thread:with-mutex ((slot-value tf 'lock))
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
                                  (or time
                                      (stamp (or (car (last up-transforms))
                                                 (car down-transforms))))
                                  (translation result-tf)
                                  (rotation result-tf)))))))

(defmethod set-transform ((tf transformer) (transform stamped-transform) &key suppress-callbacks)
  (with-slots (transforms set-transform-callbacks lock) tf
    (sb-thread:with-mutex (lock)
      (let ((cache (gethash (child-frame-id transform) transforms)))
        (when (or (not cache) (eql cache 'parent))
          (setf cache (make-instance 'transform-cache))
          (setf (gethash (ensure-fully-qualified-name (child-frame-id transform))
                         transforms) cache))
        (cache-transform cache transform))
      (unless (gethash (frame-id transform) transforms)
        (setf (gethash (frame-id transform) transforms) 'parent)))
    (unless suppress-callbacks
      (execute-set-callbacks tf))))

(defmethod wait-for-transform ((tf transformer) &key target-frame source-frame time)
  (let ((cond-var (sb-thread:make-waitqueue))
        (lock (sb-thread:make-mutex))
        (waiter-name (gensym))
        (target-frame (ensure-fully-qualified-name target-frame))
        (source-frame (ensure-fully-qualified-name source-frame))
        (time (ensure-null-time time)))
    (check-transform-exists tf target-frame)
    (check-transform-exists tf source-frame)
    (flet ((on-set-transform ()
             (sb-thread:with-mutex (lock)
               (sb-thread:condition-broadcast cond-var))))
      (unwind-protect
           (unless (can-transform
                         tf :time time
                         :target-frame target-frame
                         :source-frame source-frame)
             (add-set-callback tf waiter-name #'on-set-transform)
             (loop
               do (sb-thread:with-mutex (lock)
                    (sb-thread:condition-wait cond-var lock))
               until (can-transform
                         tf :time time
                         :target-frame target-frame
                         :source-frame source-frame)))
        (remove-set-callback tf waiter-name)))))

(defmethod transform-pose ((tf transformer) &key target-frame pose time)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame))
        (time (ensure-null-time time)))
    (check-transform-exists tf target-frame)
    (let ((transform (lookup-transform
                      tf
                      :target-frame target-frame
                      :source-frame (frame-id pose)
                      :time time)))
      (assert transform () "Transform from `~a' to `~a' not found."
              (frame-id pose) target-frame)
      (change-class (cl-transforms:transform-pose transform pose)
                    'pose-stamped
                    :frame-id target-frame
                    :stamp (stamp transform)))))

(defmethod transform-point ((tf transformer) &key target-frame point time)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame))
        (time (ensure-null-time time)))
    (check-transform-exists tf target-frame)
    (let ((transform (lookup-transform
                      tf
                      :target-frame target-frame
                      :source-frame (frame-id point)
                      :time time)))
      (assert transform () "Transform from `~a' to `~a' not found."
              (frame-id point) target-frame)
      (change-class (cl-transforms:transform-point transform point)
                    'point-stamped
                    :frame-id target-frame
                    :stamp (stamp transform)))))

(defun get-transforms-to-root (transforms frame-id time &optional result)
  "Returns the list of transforms from `frame-id' up to the root of
  the tree."
  (let ((current-cache (gethash frame-id transforms))
        (time (ensure-null-time time)))
    (if (and current-cache (typep current-cache 'transform-cache))
        (let ((current-tf (get-cached-transform (gethash frame-id transforms) time)))
          (get-transforms-to-root transforms (frame-id current-tf)
                                  time (cons current-tf result)))
        result)))

(defun execute-set-callbacks (tf)
  (with-slots (set-transform-callbacks) tf
    (map 'nil (cl-utils:compose #'funcall #'cdr) set-transform-callbacks)))

(defun add-set-callback (tf name callback)
  (with-slots (set-transform-callbacks) tf
    (pushnew (cons name callback) set-transform-callbacks)))

(defun remove-set-callback (tf name)
  (with-slots (set-transform-callbacks) tf
    (setf set-transform-callbacks (remove name set-transform-callbacks
                                          :key #'car))))

(defun ensure-fully-qualified-name (frame-id)
  "Makes sure that the first character in `frame-id' is a '/'"
  (declare (type string frame-id))
  (if (eql (elt frame-id 0) #\/)
      frame-id
      (concatenate 'string "/" frame-id)))

(defun ensure-null-time (time)
  "Makes sure that time is NIL if it is either NIL or 0"
  (cond ((null time) time)
        ((and (numberp time)
              (= time 0.0))
         nil)
        (t time)))

(defun check-transform-exists (transformer frame-id)
  (unless (gethash frame-id (transforms transformer))
    (error 'tf-lookup-error :frame frame-id))
  t)
