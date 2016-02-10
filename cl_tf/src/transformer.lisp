;;;
;;; Copyright (c) 2010, Lorenz Moesenlechner <moesenle@in.tum.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors 
;;;       may be used to endorse or promote products derived from this software 
;;;       without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-tf)

(defclass transformer ()
  ((transforms :initform (make-hash-table :test 'equal)
               :reader transforms)
   (set-transform-callbacks :initform nil)
   (lock :initform (sb-thread:make-mutex))
   (tf-prefix :initarg :tf-prefix :initform "/" :reader tf-prefix)))

(defgeneric can-transform (tf &key target-frame source-frame time))

(defgeneric lookup-cached-transform (tf &key target-frame source-frame time))

(defgeneric wait-for-transform (tf &key target-frame source-frame time))

(defgeneric set-transform (tf transform &key suppress-callbacks))

(defgeneric transform-pose (tf &key target-frame pose))

(defgeneric transform-point (tf &key target-frame point))


(defun execute-changed-callbacks (tf)
  (with-slots (set-transform-callbacks) tf
    (map 'nil (cl-utils:compose #'funcall #'cdr) set-transform-callbacks)))

(defun add-new-transform-stamped-callback (tf name callback)
  (with-slots (set-transform-callbacks) tf
    (pushnew (cons name callback) set-transform-callbacks)))

(defun remove-new-transform-stamped-callback (tf name)
  (with-slots (set-transform-callbacks) tf
    (setf set-transform-callbacks (remove name set-transform-callbacks
                                          :key #'car))))

(defmacro with-new-transform-stamped-callback
    ((transformer callback &key (name (gensym "NEW-TRANSFORM-CALLBACK-")))
     &body body)
  "Executes `body' in such a way that each time a new transform is available
to the `transformer' `callback' is called."
  `(unwind-protect
        (progn
          (add-new-transform-stamped-callback ,transformer ',name ,callback)
          ,@body)
     (remove-new-transform-stamped-callback ,transformer ',name)))


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
      (extrapolation-error ()
        nil)
      (lookup-error ()
        nil))))

(defmethod lookup-cached-transform ((tf transformer) &key target-frame source-frame time)
  (check-type target-frame string)
  (check-type source-frame string)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (source-frame (ensure-fully-qualified-name source-frame (tf-prefix tf)))
        (time (ensure-null-time time)))
    (when (equal target-frame source-frame)
      (return-from lookup-cached-transform
        (make-transform-stamped
         target-frame source-frame (ros-time)
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
            (error 'connectivity-error :description (format nil "The frames ~a
and ~a are not connected in the reference frame tree" source-frame target-frame)))
          (make-transform-stamped
           target-frame source-frame
           (or time
               (stamp (or (car (last up-transforms))
                          (car down-transforms))))
           (translation result-tf)
           (rotation result-tf)))))))

(defmethod lookup-transform ((tf transformer) target-frame source-frame
                             &key time timeout target-time fixed-frame)
  (declare (type string target-frame source-frame)
           (type (or number null) time timeout))
  (when (or target-time fixed-frame)
    (warn "LOOKUP-TRANSFORM of CL-TF:TRANSFORMER does not support
TARGET-TIME or FIXED-FRAME arguments."))
  (let ((timeout (or timeout 0.0))) ; check in case timeout explicitly set to NIL
    (if (wait-for-transform tf :time time :timeout timeout
                               :source-frame source-frame :target-frame target-frame)
        (lookup-cached-transform tf :time time
                                    :source-frame source-frame :target-frame target-frame)
        (error 'timeout-error :description
               (format nil "No transform was published between frames ~a and ~a"
                       source-frame target-frame)))))

(defmethod set-transform ((tf transformer) (transform transform-stamped) &key suppress-callbacks)
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
           cache (make-transform-stamped
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
      (or (can-transform tf
                         :time time
                         :target-frame target-frame
                         :source-frame source-frame)
          (with-new-transform-stamped-callback (tf #'on-set-transform)
            (if timeout
                (let ((timer (sb-ext:make-timer
                              (lambda ()
                                (return-from wait-for-transform nil)))))
                  (sb-ext:schedule-timer timer timeout)
                  (unwind-protect (do-wait-for-transform)
                    (sb-ext:unschedule-timer timer)))
                (do-wait-for-transform)))))))

(defmethod transform-pose-stamped ((tf transformer)
                                   &key target-frame pose timeout use-current-ros-time)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (time (if use-current-ros-time
                  (roslisp:ros-time)
                  (ensure-null-time (stamp pose))))
        (timeout (or timeout 0.0))) ; check in case timeout explicitly set to NIL
    (let ((transform (lookup-transform tf target-frame (frame-id pose)
                                       :time time :timeout timeout)))
      (assert transform () "Transform from `~a' to `~a' not found."
              (frame-id pose) target-frame)
      (change-class (cl-transforms:transform-pose transform pose)
                    'pose-stamped
                    :frame-id target-frame
                    :stamp (stamp transform)))))

(defmethod transform-pose ((tf transformer) &key target-frame pose)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (time (ensure-null-time (stamp pose))))
    (let ((transform (lookup-cached-transform
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

(defmethod transform-point-stamped ((tf transformer)
                                    &key target-frame point timeout use-current-ros-time)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (time (if use-current-ros-time
                  (roslisp:ros-time)
                  (ensure-null-time (stamp point))))
        (timeout (or timeout 0.0))) ; check in case timeout explicitly set to NIL
    (check-transform-exists tf target-frame)
    (let ((transform (lookup-transform tf target-frame (frame-id point)
                                       :time time :timeout timeout)))
      (assert transform () "Transform from `~a' to `~a' not found."
              (frame-id point) target-frame)
      (change-class (cl-transforms:transform-point transform point)
                    'point-stamped
                    :frame-id target-frame
                    :stamp (stamp transform)))))

(defmethod transform-point ((tf transformer) &key target-frame point)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((target-frame (ensure-fully-qualified-name target-frame (tf-prefix tf)))
        (time (ensure-null-time (stamp point))))
    (check-transform-exists tf target-frame)
    (let ((transform (lookup-cached-transform
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
    (error 'lookup-error :description
           (format nil "The frame ~a is not in the graph" frame-id)))
  t)
