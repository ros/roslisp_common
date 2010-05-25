(defpackage :test-transforms
  (:use :cl :cl-utils :cl-transforms :cl-test))

(in-package :test-transforms)

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
  (make-3d-vector (rand *range*) (rand *range*) (rand *range*)))


(defun vec-close-to (v1 v2)
  (let ((dx (- (x v1) (x v2)))
        (dy (- (y v1) (y v2)))
        (dz (- (z v1) (z v2))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-rotation () (normalize (random-quaternion)))

(defun random-transform ()
  (make-transform (random-vec) (random-rotation)))

(defun random-pose ()
  (make-pose (random-vec) (random-rotation)))

(defun pose-close-to (p1 p2)
  (and (vec-close-to (origin p1) (origin p2))
       (approx-equal (orientation p1) (orientation p2))))

(repeat *num-tests*
  (let ((tr1 (random-transform))
        (tr2 (random-transform))
        (p1 (random-vec))
        (p2 (random-vec))
        (q (random-rotation))
        (a (random 1.0)))

    ;; Check that transform composition works
    (check-equal (transform-point (transform* tr1 tr2) p1)
                 (transform-point tr1 (transform-point tr2 p1))
                 #'vec-close-to)

    
    ;; Check that transforms are convex
    (check-equal (v+ (v* (transform-point tr1 p1) a)
                     (v* (transform-point tr1 p2) (- 1 a)))
                 (transform-point tr1 (v+ (v* p1 a) (v* p2 (- 1 a))))
                 #'vec-close-to)

    ;; Check that translations are additions
    (check-equal (transform-point (make-transform p2 1.0) p1)
                 (v+ p1 p2) #'vec-close-to)

    ;; Check that rotations work as expected
    (check-equal (transform-point (make-transform #(0 0 0) q) p1)
                 (rotate q p1) #'vec-close-to)
    
    
    ;; TODO check transform-pose


    ))


