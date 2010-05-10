(defpackage :test-quaternions
  (:use :cl :cl-utils :quaternions :cl-test))

(in-package :test-quaternions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic quaternion ops
;; We're going to test things by generating random 
;; quaternions and checking some standard
;; algebraic properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *range* 100.0)
(defvar *num-tests* 100)
(defvar *tol* 1e-3)

(defun rand (r)
  (- (random (* 2 r)) r))

(defun random-quaternion ()
  (make-quaternion (rand *range*) (rand *range*) (rand *range*) (rand *range*)))

(defun rand-arg ()
  (if (zerop (random 5))
      (rand *range*)
      (random-quaternion)))


(defun approx-equal (q1 q2)
  (< (/ (q-norm (q- q1 q2)) (+ (q-norm q1) (q-norm q2))) *tol*))

(repeat *num-tests*
  (let ((q1 (random-quaternion))
        (q2 (random-quaternion))
        (q3 (random-quaternion)))

    ;; Check associativity of multiplication
    (check-equal (q* q1 (q* q2 q3))
                 (q* (q* q1 q2) q3)
                 #'approx-equal)

    ;; Check inverse properties
    (check-equal (q* q1 (q-inv q1)) 1.0 #'approx-equal)
    (check-equal (q* (q-inv q2) (q-inv q1)) (q-inv (q* q1 q2)) #'approx-equal)
    (check-equal (q* q1 1.0) q1 #'approx-equal)
    (check-equal (q* 1.0 q1) q1 #'approx-equal)

    ;; Check distributivity
    (check-equal (q* q1 (q+ q2 q3)) (q+ (q* q1 q2) (q* q1 q3)) #'approx-equal)

    ;; Subtraction
    (check-equal (q- q1 q2 q3) (q+ (q* q2 -1) q1 (q* -1 q3)) #'approx-equal)

))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quaternions-as-rotations tests
;; As before, generate random quaternions and vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-vec ()
  (let ((v (make-array 3 :element-type 'quaternion-coefficient)))
    (setf (aref v 0) (rand *range*)
          (aref v 1) (rand *range*)
          (aref v 2) (rand *range*))
    v))


(defun vec-close-to (v1 v2)
  (let ((dx (- (aref v1 0) (aref v2 0)))
        (dy (- (aref v1 1) (aref v2 1)))
        (dz (- (aref v1 2) (aref v2 2))))
    (< (+ (* dx dx) (* dy dy) (* dz dz))
       *tol*)))


(repeat *num-tests*
  (let ((q1 (normalize (random-quaternion)))
        (q2 (normalize (random-quaternion)))
        (v (random-vec)))

    ;; Check that quaternion multiplication corresponds to composition
    (check-equal (rotate (q* q1 q2) v) (rotate q1 (rotate q2 v)) #'vec-close-to)

    ;; Check that inversion works
    (check-equal (rotate q1 (rotate (q-inv q1) v)) v #'vec-close-to)
    )
  )