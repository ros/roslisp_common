
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
  ((newest-stamp :initform nil :accessor newest-stamp)
   (fill-pointer :initform 0 :accessor cache-fill-pointer)
   (transforms-cache :accessor transforms-cache)))

(defgeneric cache-transform (cache transform)
  (:documentation "Cache a transform."))

(defgeneric get-cached-transform (cache timestamp &key interpolate)
  (:documentation "Find the transform for a specific timestamp. When
  `interpolate' is T, return an interpolated transform, otherwise,
  return the transformation whose timestamp is closest to
  `timestamp'."))

(defmethod initialize-instance :after ((tf-cache transform-cache) &key)
  (setf (slot-value tf-cache 'cache)
        (make-array (slot-value tf-cache 'cache-size) :element-type 'cache-entry :adjustable 0)))

(defmethod cache-transform ((tf-cache transform-cache) transform)
  (with-slots (cache-size cache) tf-cache
    (let* ((cache-entry-index (truncate (mod (stamp transform) cache-size)))
           (cache-entry (aref cache cache-entry-index)))
      (when (> (- (stamp transform) (newest-stamp cache-entry))
               cache-size)
        (gc-cache-entry cache-entry))
      (cache-transform cache-entry transform))))

(defmethod get-cached-transform ((tf-cache transform-cache) timestamp &key (interpolate t))
  (with-slots (cache-size cache) tf-cache
    (let* ((cache-entry-index (truncate (mod timestamp cache-size)))
           (cache-entry (aref cache cache-entry-index)))
      (when (> (abs (- timestamp (newest-stamp cache-entry)))
               cache-size)
          (error 'tf-cache-error
                 :description "Requested timestamp points to the future. Cannot transform."))
      (get-cached-transform cache-entry timestamp :interpolate interpolate))))

(defun gc-cache-entry (cache-entry)
  (with-slots (fill-pointer) cache-entry
    (setf fill-pointer 0)
    ;; TODO: Maybe shrink transforms-cache again
    ))

(defmethod initialize-instance :after ((cache-entry cache-entry) &key)
  (setf (slot-value cache-entry 'transforms-cache)
        (make-array +initial-cache-size+ :element-type 'stamped-transform
                    :initial-element (make-instance 'stamped-transform))))

(defmethod cache-transform ((cache-entry cache-entry) transform)
  (let ((cache-size (array-dimension (transforms-cache cache-entry) 0)))
    (assert (> (stamp transform) (newest-stamp cache-entry)))
    (when (eql (fill-pointer cache-entry) cache-size)
      (setf (transforms-cache cache-entry)
            (adjust-array (transforms-cache cache-entry)
                          (truncate (* cache-size +cache-adjust-factor+)))))
    (setf (aref (transforms-cache cache-entry) (fill-pointer cache-entry))
          transform)
    (setf (newest-stamp cache-entry) (stamp transform))
    (incf (fill-pointer cache-entry))))

(defmethod get-cached-transform ((cache-entry cache-entry) timestamp &key (interpolate t))
  (with-slots (newest-stamp fill-pointer transforms-cache) cache-entry
    (when (or (> timestamp newest-stamp)
              (< timestamp (stamp (aref transforms-cache 0))))
      (error 'tf-cache-error
             :description "The requested time stamp does not point into the cache."))
    (multiple-value-bind (lower upper)
        (binary-search timestamp
                       (make-array fill-pointer :displaced-to  transforms-cache)
                       :key #'stamp)
      (cond (interpolate
             (let ((ratio (/ (- timestamp (stamp lower))
                             (- (stamp upper) (stamp lower)))))
               (make-stamped-transform
                (frame-id lower)
                (child-frame-id lower)
                timestamp
                (interpolate-vector (translation lower) (translation upper) ratio)
                (slerp (rotation lower) (rotation upper) ratio))))
            (t
             (if (< (abs (- timestamp lower))
                    (abs (- timestamp upper)))
                 lower upper))))))

