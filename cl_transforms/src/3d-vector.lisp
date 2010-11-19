
(in-package :cl-transforms)

(deftype vector-coefficient () '(or fixnum float double-float))

(defclass 3d-vector ()
  ((x :initarg :x :reader x :type vector-coefficient)
   (y :initarg :y :reader y :type vector-coefficient)
   (z :initarg :z :reader z :type vector-coefficient)))

(deftype point ()
  '(or 3d-vector (vector vector-coefficient 3)))


(defmethod x ((v vector))
  (aref v 0))

(defmethod y ((v vector))
  (aref v 1))

(defmethod z ((v vector))
  (aref v 2))

(defun make-3d-vector (x y z)
  (make-instance '3d-vector :x x :y y :z z))

(defmethod print-object ((v 3d-vector) strm)
  (print-unreadable-object (v strm :type t)
    (format strm "(~a, ~a, ~a)" (x v) (y v) (z v))))

(defun v-norm (v)
  "Returns the magnitude of the vector"
  (sqrt (dot-product v v)))

(defun v+ (&rest vecs)
  (reduce #'v+-pairwise vecs))

(defun v+-pairwise (v-1 v-2)
  (make-3d-vector (+ (x v-1) (x v-2))
                  (+ (y v-1) (y v-2))
                  (+ (z v-1) (z v-2))))

(defun v- (&rest vecs)
  (reduce #'v--pairwise vecs))

(defun v--pairwise (v-1 v-2)
  (make-3d-vector (- (x v-1) (x v-2))
                  (- (y v-1) (y v-2))
                  (- (z v-1) (z v-2))))

(defun v* (v scalar)
  "Multiplies every component with a scalar and returns a new vector."
  (make-3d-vector (* (x v) scalar)
                  (* (y v) scalar)
                  (* (z v) scalar)))

(defun v-inv (v)
  (v* v -1))

(defun dot-product (v-1 v-2)
  "Returns the dot-product"
  (+ (* (x v-1) (x v-2))
     (* (y v-1) (y v-2))
     (* (z v-1) (z v-2))))

(defun cross-product (v-1 v-2)
  (make-3d-vector (- (* (y v-1) (z v-2))
                     (* (z v-1) (y v-2)))
                  (- (* (z v-1) (x v-2))
                     (* (x v-1) (z v-2)))
                  (- (* (x v-1) (y v-2))
                     (* (y v-1) (x v-2)))))

(defun v-dist (v-1 v-2)
  "Returns the euclidean distance between two vectors."
  (sqrt (+ (expt (- (x v-1) (x v-2)) 2)
           (expt (- (y v-1) (y v-2)) 2)
           (expt (- (z v-1) (z v-2)) 2))))
