(in-package :cl-transforms)

(defclass transform ()
  ((translation :initarg :translation :initform (make-identity-vector)
                :reader translation :type point)
   (rotation :initarg :rotation :initform (make-identity-rotation)
             :reader rotation :type gen-quaternion))
  (:documentation "Represents a rigid affine transform of R^3, consisting of a rotation (represented as a normalized-quaternion) and translation (represented as a 3d-vector).  Object should be treated as immutable."))

(defun make-transform (translation rotation &key (validate-args :warn))
  (make-instance 'transform
    :translation translation :rotation rotation
    :validate-args validate-args))

(defun make-identity-transform ()
  (make-transform
   (make-identity-vector)
   (make-identity-rotation)))

(defun copy-transform (transform &key translation rotation)
  (with-slots ((old-translation translation)
               (old-rotation rotation))
      transform
    (make-transform
     (or translation (copy-3d-vector old-translation))
     (or rotation (copy-quaternion old-rotation)))))

(defmethod print-object ((obj transform) strm)
  (print-unreadable-object (obj strm :type t)
    (with-slots (translation rotation) obj
      (format strm "~{~<~%   ~a~>~}" (list translation rotation)))))

(defmethod initialize-instance :after ((tr transform) &key (validate-args :warn))
  (when validate-args
    (with-slots (rotation) tr
      (unless (is-normalized rotation)
        (if (eq validate-args :warn)
            (progn
              (setq rotation (normalize rotation))
              (warn "Normalized rotation component to ~a" rotation)
              )
            (error "Rotation component ~a not normalized" rotation))))))

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
                            (q* (rotation trans) (rotation prev))))
          (reverse transforms)))

(defun transform-diff (transform-To transform-From)
  "Return the transform that, when left-multiplied, will convert transform-From into transform-To."
  (transform* transform-To (transform-inv transform-From)))

(defun transform-point (trans p)
  (declare (type transform trans) (type point p))
  (v+ (translation trans) (rotate (rotation trans) p)))

(defgeneric transform (tr x)
  (:method ((tr transform) (p 3d-vector))
    (transform-point tr p))
  (:method ((tr transform) (p vector))
    (assert (= 3 (length p)))
    (transform-point tr p)))
