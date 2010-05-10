(in-package :cl-transforms)

(deftype 3d-vector () '(vector quaternion-coefficient 3))

(defparameter *tolerance* 1e-6)

(defun axis-angle->quaternion (axis angle)
  "Return quaternion corresponding to rotation about axis by angle"
  (declare (type 3d-vector axis)
           (type quaternion-coefficient angle))
  (let ((c (cos (/ angle 2)))
        (s (sin (/ angle 2))))
    (make-instance 'quaternion :w c
                   :x (* s (aref axis 0)) :y (* s (aref axis 1))
                   :z (* s (aref axis 2)))))

(defun normalize (q)
  "Crude normalization by just dividing all coefficients by the norm.
This guarantees that Q represents a rotation."
  (let ((n (q-norm q)))
    (when (< n *tolerance*)
      (error "Attempted to normalize quaternion ~a with norm ~a" q n))
    (make-instance 'quaternion :x (/ (x q) n) :y (/ (y q) n)
                   :z (/ (z q) n) :w (/ (w q) n))))

(defun is-normalized (q)
  (declare (type gen-quaternion q))
  (< (abs (- (squared-norm q) 1.0)) *tolerance*))


(defun rotate (q v &key (normalize :check))
  (declare (type 3d-vector v) (type gen-quaternion q))
  (cond
    ((eq normalize :check) (assert (is-normalized q)))
    ((eq normalize t) (setq q (normalize q))))
  (with-readers (x y z w) q
    (let ((t2 (* w x))
          (t3 (* w y))
          (t4 (* w z))
          (t5 (- (* x x)))
          (t6 (* x y))
          (t7 (* x z))
          (t8 (- (* y y)))
          (t9 (* y z))
          (t10 (- (* z z)))
          (v0 (aref v 0))
          (v1 (aref v 1))
          (v2 (aref v 2))
          (v (make-array 3 :element-type 'quaternion-coefficient)))
      (setf (aref v 0)
            (* 2
               (+ (* v0 (+ t8 t10 0.5))
                  (* v1 (- t6 t4))
                  (* v2 (+ t3 t7))))
            (aref v 1)
            (* 2
               (+ (* v0 (+ t4 t6))
                  (* v1 (+ t5 t10 0.5))
                  (* v2 (- t9 t2))))
            (aref v 2)
            (* 2
               (+ (* v0 (- t7 t3))
                  (* v1 (+ t2 t9))
                  (* v2 (+ t5 t8 0.5)))))
      v)))