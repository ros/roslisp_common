
(in-package :cl-tf)

;;;
;;; We implement a two-level cache. The first level is on a per-second
;;; basis and the second level is a temporally ordered array that
;;; keeps all transformations between the same frames (frame id and
;;; child id equal) for one second.
;;;
;;; The first cache level makes garbage collection very fast. We
;;; collect all transforms of the complete 1s interval at once.
;;;
;;; The second level cache is a fixed-sized array which is resized
;;; when needed. We assume that transforms come in at relatively fixed
;;; rates. That means only a few resize operations should be needed at
;;; the beginning.
;;; 

(defconstant +initial-cache-size+ 20)
(defconstant +cache-adjust-factor+ 1.5)

(define-condition tf-cache-error (error)
  ((description :initarg :description)))

(defclass transform-cache ()
  ((cache-size :initarg :cache-size :initform 10 :reader cache-size)
   (cache :accessor cache)))

(defclass cache-entry ()
  ((newest-stamp :initform 0 :accessor newest-stamp)
   (fill-pointer :initform 0 :accessor cache-fill-pointer)
   (transforms-cache :accessor transforms-cache)))

(defgeneric cache-transform (cache transform)
  (:documentation "Cache a transform."))

(defgeneric get-cached-transform (cache time &key interpolate)
  (:documentation "Find the transform for a specific time. When
  `interpolate' is T, return an interpolated transform, otherwise,
  return the transformation with a time that is closest to
  `time'."))

(defgeneric cache-empty (cache-entry)
  (:method ((cache-entry cache-entry))
    (eql (cache-fill-pointer cache-entry) 0)))

(defmethod initialize-instance :after ((tf-cache transform-cache) &key)
  (setf (slot-value tf-cache 'cache)
        (make-array (slot-value tf-cache 'cache-size) :element-type 'cache-entry
                    :initial-contents (loop repeat (slot-value tf-cache 'cache-size)
                                            collecting (make-instance 'cache-entry)))))

(defmethod cache-transform ((tf-cache transform-cache) transform)
  (with-slots (cache-size cache) tf-cache
    (declare (type (simple-array cache-entry 1) cache))
    (let* ((cache-entry-index (truncate (mod (stamp transform) cache-size)))
           (cache-entry (aref cache cache-entry-index)))
      (when (> (- (stamp transform) (newest-stamp cache-entry)) 1)
        ;; When writing the first entry into a cache, we need to also
        ;; write it into the previous one to make interpolation
        ;; work. Otherwise, we cannot request transforms between the
        ;; last transform of the previous cache entry and the first
        ;; transform of the current cache entry.
        (let ((prev-entry-index (truncate (mod (1- (stamp transform)) cache-size))))
          (cache-transform (aref cache prev-entry-index) transform))
        (gc-cache-entry cache-entry))
      (cache-transform cache-entry transform))))

(defmethod get-cached-transform ((tf-cache transform-cache) time &key (interpolate t))
  (with-slots (cache-size cache) tf-cache
    (unless time
      (return-from get-cached-transform
        (get-cached-transform
         (loop for cache-entry across cache
               with latest-stamp = 0
               with latest-cache-entry = cache-entry
               when (and (>= (newest-stamp cache-entry) latest-stamp)
                         (not (cache-empty cache-entry)))
                 do (setf latest-stamp (newest-stamp cache-entry)
                          latest-cache-entry cache-entry)
               finally (return latest-cache-entry))
         time :interpolate interpolate)))
    (let* ((cache-entry-index (truncate (mod time cache-size)))
           (cache-entry (aref cache cache-entry-index)))
      (when (> (abs (- time (newest-stamp cache-entry)))
               cache-size)
          (error 'tf-cache-error
                 :description "Requested time points to the future. Cannot transform."))
      (get-cached-transform cache-entry time :interpolate interpolate))))

(defun gc-cache-entry (cache-entry)
  (with-slots (fill-pointer newest-stamp) cache-entry
    (setf fill-pointer 0)
    (setf newest-stamp 0)
    ;; TODO: Maybe shrink transforms-cache
    ))

(defmethod initialize-instance :after ((cache-entry cache-entry) &key)
  (setf (slot-value cache-entry 'transforms-cache)
        (make-array +initial-cache-size+ :element-type '(or null stamped-transform)
                    :initial-element nil)))

(defmethod cache-transform ((cache-entry cache-entry) transform)
  (let ((cache-size (array-dimension (transforms-cache cache-entry) 0))
        (cache (transforms-cache cache-entry)))
    (declare (type (simple-array (or null stamped-transform) 1) cache))
    (unless (>= (stamp transform) (newest-stamp cache-entry))
      (ros-debug
       (cl-tf cache)
       "Transform `~a' to `~a'. Timestamp `~a' earlyer than newest timestamp `~a'. Ignoring transform."
       (frame-id transform) (child-frame-id transform) (stamp transform)
       (newest-stamp cache-entry))
      (return-from cache-transform))
    (when (eql (cache-fill-pointer cache-entry) cache-size)
      (setf cache (resize-transforms-cache
                   cache (* cache-size +cache-adjust-factor+)))
      (setf (transforms-cache cache-entry) cache))
    (setf (aref cache (cache-fill-pointer cache-entry))
          transform)
    (setf (newest-stamp cache-entry) (stamp transform))
    (incf (cache-fill-pointer cache-entry))))

(defmethod get-cached-transform ((cache-entry cache-entry) time &key (interpolate t))
  (with-slots (newest-stamp fill-pointer transforms-cache) cache-entry
    (declare (type (simple-array * 1) transforms-cache))
    (assert (not (cache-empty cache-entry)))
    (unless time
      ;; Early exit. When no time is specified, return the newest stamp
      (return-from get-cached-transform
        (aref transforms-cache (1- fill-pointer))))
    (when (or (> time newest-stamp)
              (< time (stamp (aref transforms-cache 0))))
      (error 'tf-cache-error
             :description "The requested time stamp does not point into the cache."))
    (multiple-value-bind (lower upper)
        (binary-search time transforms-cache
                       :end (1- fill-pointer) :key #'stamp)
      (cond ((not lower)
             (error "The cache is in a weird state. Although the
             timestamp should be in the cache no corresponding
             transforms could be found. Please file a ticket."))
            ((eql (stamp lower) time)
             lower)
            ((eql (stamp upper) time)
             upper)
            (interpolate
             (let ((ratio (/ (- time (stamp lower))
                             (- (stamp upper) (stamp lower)))))
               (make-stamped-transform
                (frame-id lower)
                (child-frame-id lower)
                time
                (interpolate-vector (translation lower) (translation upper) ratio)
                (slerp (rotation lower) (rotation upper) ratio))))
            (t
             (if (< (abs (- time lower))
                    (abs (- time upper)))
                 lower upper))))))

(defun resize-transforms-cache (cache new-size)
  (declare (type (simple-array * 1) cache))
  (let ((result (make-array (truncate new-size) :element-type (array-element-type cache))))
    (map-into result #'identity cache)))
