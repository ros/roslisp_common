
(in-package :cl-tf)

(defmacro with-transforms-changed-callback
    ((tf callback &key (name (gensym "TRANSFORMS-CHANGED-CALLBACK-"))) &body body)
  `(unwind-protect
        (progn
          (add-transforms-changed-callback ,tf ',name ,callback)
          ,@body)
     (remove-transforms-changed-callback ,tf ',name)))

(define-condition tf-connectivity-error (error)
  ((source-frame :initarg :source-frame :reader source-frame)
   (target-frame :initarg :target-frame :reader target-frame)))

(define-condition tf-lookup-error (error)
  ((frame :initarg :frame :reader frame)))

(defclass transformer ()
  ((transforms :initform (make-hash-table :test 'equal)
               :reader transforms)
   (set-transform-callbacks :initform nil)
   (lock :initform (sb-thread:make-mutex))
   (tf-prefix :initarg :tf-prefix :initform "/" :reader tf-prefix)))

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
        (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
              (source-frame (ensure-fully-qualified-name source-frame (tf-prefix tf)))
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
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (source-frame (ensure-fully-qualified-name source-frame (tf-prefix tf)))
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
      (let ((frame-id (ensure-fully-qualified-name (frame-id transform)))
            (child-frame-id (ensure-fully-qualified-name (child-frame-id transform))))
        (let ((cache (gethash child-frame-id transforms)))
          (when (or (not cache) (eql cache 'parent))
            (setf cache (make-instance 'transform-cache))
            (setf (gethash (ensure-fully-qualified-name child-frame-id)
                           transforms) cache))
          (cache-transform
           cache (make-stamped-transform
                  frame-id child-frame-id (stamp transform)
                  (translation transform) (rotation transform))))
        (unless (gethash frame-id transforms)
          (setf (gethash frame-id transforms) 'parent))))
    (unless suppress-callbacks
      (execute-changed-callbacks tf))))

(defmethod wait-for-transform ((tf transformer) &key target-frame source-frame time timeout)
  (let ((cond-var (sb-thread:make-waitqueue))
        (lock (sb-thread:make-mutex))
        (target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (source-frame (ensure-fully-qualified-name source-frame (tf-prefix tf)))
        (time (ensure-null-time time)))
    (flet ((on-set-transform ()
             (sb-thread:with-mutex (lock)
               (sb-thread:condition-broadcast cond-var)))
           (do-wait-for-transform ()
             (loop
               do (sb-thread:with-mutex (lock)
                    (sb-thread:condition-wait cond-var lock))
               until (can-transform
                      tf :time time
                         :target-frame target-frame
                         :source-frame source-frame)
               finally (return t))))
      (or (can-transform
                    tf :time time
                       :target-frame target-frame
                       :source-frame source-frame)
          (with-transforms-changed-callback (tf #'on-set-transform)
            (if timeout
                (let ((timer (sb-ext:make-timer
                              (lambda ()
                                (return-from wait-for-transform nil)))))
                  (sb-ext:schedule-timer timer timeout)
                  (unwind-protect (do-wait-for-transform)
                    (sb-ext:unschedule-timer timer)))
                (do-wait-for-transform)))))))

(defmethod transform-pose ((tf transformer) &key target-frame pose)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (time (ensure-null-time (stamp pose))))
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

(defmethod transform-point ((tf transformer) &key target-frame point)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (time (ensure-null-time (stamp point))))
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
  ;; We need to ensure the fully qualified name for every frame along
  ;; the tree because not every tf publisher might publish the fully
  ;; qualified id.
  (let ((current-cache (gethash frame-id transforms))
        (time (ensure-null-time time)))
    (if (and current-cache (typep current-cache 'transform-cache))
        (let ((current-tf (get-cached-transform (gethash frame-id transforms) time)))
          (get-transforms-to-root transforms (frame-id current-tf)
                                  time (cons current-tf result)))
        result)))

(defun execute-changed-callbacks (tf)
  (with-slots (set-transform-callbacks) tf
    (map 'nil (cl-utils:compose #'funcall #'cdr) set-transform-callbacks)))

(defun add-transforms-changed-callback (tf name callback)
  (with-slots (set-transform-callbacks) tf
    (pushnew (cons name callback) set-transform-callbacks)))

(defun remove-transforms-changed-callback (tf name)
  (with-slots (set-transform-callbacks) tf
    (setf set-transform-callbacks (remove name set-transform-callbacks
                                          :key #'car))))

(defun ensure-fully-qualified-name (frame-id &optional (tf-prefix "/"))
  "Makes sure that the first character in `frame-id' is set to `tf-prefix'"
  (declare (type string frame-id tf-prefix))
  (if (eql (elt frame-id 0) #\/)
      frame-id
      (concatenate 'string tf-prefix frame-id)))

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
