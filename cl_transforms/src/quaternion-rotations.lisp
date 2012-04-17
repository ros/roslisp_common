(in-package :cl-transforms)

(defparameter *tolerance* 1e-6)

(defun make-identity-rotation (&optional (type-template 0.0d0))
  (make-quaternion
   (float 0.0d0 type-template)
   (float 0.0d0 type-template)
   (float 0.0d0 type-template)
   (float 1.0d0 type-template)))

(defun axis-angle->quaternion (axis angle)
  "Return quaternion corresponding to rotation about axis by angle"
  (declare (type point axis)
           (type quaternion-coefficient angle))
  (let ((c (cos (/ angle 2)))
        (s (sin (/ angle 2)))
        (axis (normalize-axis axis)))
    (make-quaternion (* s (x axis))
                     (* s (y axis))
                     (* s (z axis))
                     c)))

(defun quaternion->axis-angle (q)
  "convert quaternion to axis and angle.  Assumes q is normalized."
  (values
    (make-3d-vector (x q) (y q) (z q))
    ;; If we take the acos of a number >1 we get a complex
    ;; number. Numbers can become greater than 1 due to numerical
    ;; inaccuracies. We fix this by truncating the number if it is
    ;; >1. Handling of numbers <1 accordingly.
    (let ((w-normalized (cond ((> (w q) 1.0d0) 1.0d0)
                              ((< (w q) -1.0d0) -1.0d0)
                              (t (w q)))))
      (* 2 (acos w-normalized)))))

(defun euler->quaternion (&key (ax 0.0) (ay 0.0) (az 0.0))
  "create a quaternion from euler angles"
  (let ((phi (* ax 0.5))
        (the (* ay 0.5))
        (psi (* az 0.5)))
    (make-quaternion (- (* (sin phi) (cos the) (cos psi)) (* (cos phi) (sin the) (sin psi)))
                     (+ (* (cos phi) (sin the) (cos psi)) (* (sin phi) (cos the) (sin psi)))
                     (- (* (cos phi) (cos the) (sin psi)) (* (sin phi) (sin the) (cos psi)))
                     (+ (* (cos phi) (cos the) (cos psi)) (* (sin phi) (sin the) (sin psi))))))

(defun get-yaw (quaternion)
  (with-slots (x y z w) quaternion
    (atan (* 2 (+ (* x y) (* w z)))
          (+ (* w w) (* x x) (* -1 y y) (* -1 z z)))))

(defun yaw (angle)
  (axis-angle->quaternion #(0 0 1) angle))

(defun normalize (q)
  "Crude normalization by just dividing all coefficients by the norm.
This guarantees that Q represents a rotation."
  (let ((n (q-norm q)))
    (when (< n *tolerance*)
      (error "Attempted to normalize quaternion ~a with norm ~a" q n))
    (make-instance 'quaternion :x (/ (x q) n) :y (/ (y q) n)
                   :z (/ (z q) n) :w (/ (w q) n))))

(defun normalize-axis (axis)
  "Normalize the axis vector (if necessary)"
  (declare (type point axis))
  (with-readers (x y z) axis
    (let ((squared-norm (+ (* x x) (* y y) (* z z))))
      (cond 
        ((close-to squared-norm 1.0 *tolerance*)  axis)
        ((<= squared-norm 0) (error "Can't normalize ~a" axis))
        (t (let ((norm (sqrt squared-norm)))
             (make-3d-vector (/ x norm) (/ y norm) (/ z norm))))))))


(defun is-normalized (q)
  (declare (type gen-quaternion q))
  (< (abs (- (squared-norm q) 1.0)) *tolerance*))


(defun rotate (q v &key (normalize :check))
  (declare (type point v) (type gen-quaternion q))
  (cond
    ((eq normalize :check) (assert (is-normalized q)))
    ((eq normalize t) (setq q (normalize q))))
  (with-readers (x y z w) q
    (with-readers ((v0 x) (v1 y) (v2 z)) v
      (let ((t2 (* w x))
            (t3 (* w y))
            (t4 (* w z))
            (t5 (- (* x x)))
            (t6 (* x y))
            (t7 (* x z))
            (t8 (- (* y y)))
            (t9 (* y z))
            (t10 (- (* z z))))
        (make-3d-vector
         (* 2
            (+ (* v0 (+ t8 t10 0.5))
               (* v1 (- t6 t4))
               (* v2 (+ t3 t7))))
         (* 2
            (+ (* v0 (+ t4 t6))
               (* v1 (+ t5 t10 0.5))
               (* v2 (- t9 t2))))
         (* 2
                 (+ (* v0 (- t7 t3))
                    (* v1 (+ t2 t9))
                    (* v2 (+ t5 t8 0.5)))))))))

(defun angle-between-quaternions (q1 q2)
  "Returns two values: the angle between quaternions `q1' and `q2' and
   the rotation axis."
  (multiple-value-bind (axis angle)
      (quaternion->axis-angle (q* (q-inv q1) q2))
    (values angle axis)))

(defun normalize-angle (angle)
  "Ensures that `angle' is within the interval [-PI;PI)."
  (let ((2pi (* 2 pi)))
    (loop while (< angle pi) do
      (setf angle (+ angle 2pi)))
    (loop while (> angle pi) do
      (setf angle (- angle 2pi))))
  angle)
