
(in-package :cl-transforms)

(deftype quaternion-coefficient () '(or float double-float))

(defparameter *tolerance* 1e-6)
(defparameter *default-quaternion-coefficient-type* 'double-float)

(declaim (inline ensure-quaternion-coefficient-type))
(defun ensure-quaternion-coefficient-type (val)
  (ecase *default-quaternion-coefficient-type*
    (single-float (float val))
    (double-float (float val 0.0d0))))

(defclass quaternion ()
  ((x :initarg :x :reader x :type quaternion-coefficient)
   (y :initarg :y :reader y :type quaternion-coefficient)
   (z :initarg :z :reader z :type quaternion-coefficient)
   (w :initarg :w :reader w :type quaternion-coefficient))
  (:documentation "Represents a quaternion w, (x, y, z).  Must be treated as immutable.  The quaternion objects can take in real numbers as well, which are implicitly converted to a quaternion."))

(defmethod w ((q vector))
  (aref q 3))

(deftype gen-quaternion () '(or quaternion (vector quaternion-coefficient 4) real))

(defun make-quaternion (x y z w)
  "Create a quaternion.  x, y, z is the vector part and w is the scalar."
  (make-instance 'quaternion
    :x (ensure-quaternion-coefficient-type x)
    :y (ensure-quaternion-coefficient-type y)
    :z (ensure-quaternion-coefficient-type z)
    :w (ensure-quaternion-coefficient-type w)))

(defun copy-quaternion (q &key x y z w)
  (with-slots ((old-x x) (old-y y) (old-z z) (old-w w)) q
    (make-quaternion
     (or x old-x) (or y old-y) (or z old-z) (or w old-w))))

(defmethod print-object ((q quaternion) str)
  (print-unreadable-object (q str :type t) (format str "(~a ~a ~a ~a)" (x q) (y q) (z q) (w q))))

(defun q= (q1 q2)
  "Equality check checks components"
  (and (= (x q1) (x q2))
       (= (y q1) (y q2))
       (= (z q1) (z q2))
       (= (w q1) (w q2))))

(defmethod x ((r real))
  0.0)

(defmethod y ((r real))
  0.0)

(defmethod z ((r real))
  0.0)

(defmethod w ((r real))
  r)

(defun q* (&rest args)
  (cond ((null args) 1.0)
        (t (q*-pairwise (first args) (apply #'q* (rest args))))))

(defun q*-pairwise (q1 q2)
  (with-readers ((x1 x) (y1 y) (z1 z) (w1 w)) q1
    (with-readers ((x2 x) (y2 y) (z2 z) (w2 w)) q2
      (make-instance 'quaternion
                     :w (- (* w1 w2)
                           (* x1 x2)
                           (* y1 y2)
                           (* z1 z2))
                     :x (+ (- (* z1 y2))
                           (* w1 x2)
                           (* x1 w2)
                           (* y1 z2))
                     :y (+ (- (* x1 z2))
                           (* w1 y2)
                           (* y1 w2)
                           (* z1 x2))
                     :z (+ (- (* y1 x2))
                           (* w1 z2)
                           (* z1 w2)
                           (* x1 y2))))))

(defun q-inv (q)
  (with-readers (x y z w) q
    (let ((n (- (+ (* x x) (* y y) (* z z) (* w w)))))
      (make-instance 'quaternion :x (/ x n) :y (/ y n) :z (/ z n) :w (/ (- w) n)))))

(defun q-norm (q)
  (sqrt (squared-norm q)))

(declaim (inline squared-norm))
(defun squared-norm (q)
  (declare (type gen-quaternion q))
  (with-readers (x y z w) q
    (+ (* x x) (* y y) (* z z) (* w w))))

(defun q+ (&rest args)
  (cond
    ((null args) 0.0)
    (t (let ((q1 (first args))
             (q2 (apply #'q+ (rest args))))
         (make-instance 'quaternion
                        :x (+ (x q1) (x q2))
                        :y (+ (y q1) (y q2))
                        :z (+ (z q1) (z q2))
                        :w (+ (w q1) (w q2)))))))

(defun q- (q1 &rest args)
  (cond
    ((null args) (make-instance 'quaternion
                                :x (- (x q1)) :y (- (y q1)) :z (- (z q1)) :w (- (w q1))))
    (t (let* ((q2 (first args))
              (q (make-instance 'quaternion
                                :x (- (x q1) (x q2))
                                :y (- (y q1) (y q2))
                                :z (- (z q1) (z q2))
                                :w (- (w q1) (w q2)))))
         (if (cdr args)
             (apply #'q- q (cdr args))
             q)))))

(defun q-dot (q1 q2)
  (+ (* (x q1) (x q2))
     (* (y q1) (y q2))
     (* (z q1) (z q2))
     (* (w q1) (w q2))))

(defun q-scale (q scalar)
  "Returns a quaternion scaled with factor `scalar'."
  (make-quaternion (* scalar (x q))
                   (* scalar (y q))
                   (* scalar (z q))
                   (* scalar (w q))))
