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

(defun quaternion->euler (q &key (just-values nil))
  "Return a set of Euler/RPY angles corresponding to a particular quaternion.
   Parameters:
     Q : the quaternion to process
     JUST-VALUES : if nil, the result is a list with content (:AX value-x :AY value-y :AZ value-z)
                   if not nil, the result is a list with content (value-x value-y value-z)

The angles returned are in the following intervals:
  :AX (roll) between -pi and pi
  :AY (pitch) between -pi/2 and pi/2
  :AZ (yaw) between -pi and pi

Function is defined so that (except for numerical precision error)

(lambda (arg)
  (apply #'cl-tf:euler->quaternion (cl-tf:quaternion->euler arg)))

is equal to arg when arg is of type cl-tf:quaternion. Also, assuming the :AX, :AY, :AZ angles
are in the ranges given above,

(lambda (arg)
  (cl-tf:quaternion->euler (apply #'cl-tf:euler->quaternion arg)))

is equal to arg, except in cases where the pitch is +/-0.5*pi. In such cases, the resulting roll is set to zero and the resulting yaw is set to the difference (pitch pi/2) or sum (pitch pi/-2) of the yaw and roll values in arg."
  (let* ((qx (cl-transforms:x q))
         (qy (cl-transforms:y q))
         (qz (cl-transforms:z q))
         (qw (cl-transforms:w q))
         (pole-cond (- (* qw qy) (* qz qx)))
         (at-north (< (abs (- pole-cond  0.5)) 0.00001))
         (at-south (< (abs (- pole-cond -0.5)) 0.00001))
         (roll (if at-north
                 0
                 (if at-south
                   0
                   (atan (+ (* qw qx 2) (* qy qz 2)) (- 1 (* 2 qx qx) (* 2 qy qy))))))
         (pitch (if at-north
                  (/ pi 2)
                  (if at-south
                    (/ pi -2)
                    (asin (* pole-cond 2)))))
         (yaw (if at-north
                (* (atan qx qw) -2)
                (if at-south
                  (* (atan qx qw) 2)
                  (atan (+ (* qx qy 2) (* qw qz 2)) (- 1 (* 2 qy qy) (* 2 qz qz)))))))
    (if just-values
      (list roll pitch yaw)
      (list :ax roll :ay pitch :az yaw))))

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
  (when (or (>= angle pi) (< angle (- pi)))
    (let ((2pi (* 2 pi)))
      (loop while (< angle pi) do
        (setf angle (+ angle 2pi)))
      (loop while (> angle pi) do
        (setf angle (- angle 2pi)))))
  angle)
