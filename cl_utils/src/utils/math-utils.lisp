(in-package :cl-utils)

(defvar *tol* 1e-6 "Default tolerance for floating point ops")

(defun close-to (x y &optional (tol *tol*))
  "Are X and Y within absolute distance TOL of each other?"
  (declare (real x y))
  (< (abs (- x y)) tol))

(defun std (v)
  "Return standard deviation (and mean) of numbers in V"
  (let ((m (mean v)))
    (values (sqrt (/ (reduce #'+ v :key #'(lambda (x) (expt (- x m) 2))) (length v))) m)))
  
(defun mean (v)
  "Mean of numbers in v."
  (/ (reduce #'+ v) (length v)))

(defun quantile (&rest args)
  "quantile [Q] V.  Return smallest element of V that is greater than a Q proportion of them.

Q defaults to 0.5.
"
  (condlet (((rest args) (q (first args)) (v (second args)))
	    (t (q .5) (v (first args))))
    (let ((k (floor (* (length v) q))))
      (elt (sort (copy-seq v) #'<) k))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic linear algebra ops on matrices and vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric m+ (m1 m2 &key element-type)
  (:documentation "Add matrices or vectors")
  (:method ((m1 vector) (m2 vector) &key element-type)
    (orf element-type (union-array-types m1 m2))
    (let* ((n (length m1))
           (v (make-array n :element-type element-type)))
      (assert (= n (length m2)))
      (dotimes (i n v) (setf (aref v i) (+ ( aref m1 i) (aref m2 i)))))))

(defun mv* (m v &key element-type)
  "Multiply a matrix by a column vector"
  (orf element-type (union-array-types m v))
  (dsbind (k l) (array-dimensions m)
    (assert (= l (length v)))
    (let ((w (make-array l :element-type element-type)))
      (dotimes (i k w)
        (loop for j below l collecting (* (aref m i j) (aref v j)))))))

(defun inner-product (v1 v2 &key element-type)
  (orf element-type (union-array-types v1 v2))
  (let ((n (length v1)))
    (assert (= n (length v2)))
    (loop for i below n summing (* (aref v1 i) (aref v2 i)))))

(defun union-array-types (v1 v2)
  (let ((t1 (array-element-type v1))
        (t2 (array-element-type v2)))
    (cond ((eql t1 t2) t1)
          ((subtypep t1 t2) t2)
          ((subtypep t2 t1) t1)
          (t `(or ,t1 ,t2)))))

