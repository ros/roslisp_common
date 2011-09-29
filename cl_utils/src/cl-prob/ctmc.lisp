(defpackage :continuous-time-markov-chain
    (:documentation "Package for continuous time markov chain ops")
  (:use :cl-probability :cl :cl-utils)
  (:nicknames :ctmc)
  (:export
   :continuous-time-markov-chain
   :sample-jump
   :sample-future-state))

(in-package :ctmc)

(defclass continuous-time-markov-chain ()
  ((q :initarg :q :reader q)
   (alphabet :initarg :alphabet :reader alphabet :writer set-alphabet)
   (init-dist :initarg :init-dist :reader init-dist)
   (test :initarg :test :reader test :initform #'eql)
   (next-state-dists :reader next-state-dists))
  (:documentation "Initargs
:q - intensity matrix
:alphabet - list or vector of state space, where Ith entry corresponds to ith row/column in q-matrix.  Defaults to 0..n-1
:test - used for equality checking over alphabet.  Defaults to #'eql.
"))


(defmethod initialize-instance :after ((m continuous-time-markov-chain) &rest args &key q)
  (declare (ignorable args))
  (let ((n (array-dimension q 0)))

    ;; Verify Q looks ok
    (dotimes (i n)
      (let ((s 0))
	(dotimes (j n)
	  (unless (= i j)
	    (assert (>= (aref q i j) 0))
	    (incf s (aref q i j))))
	(assert (close-to s (- (aref q i i))))))

    ;; Set alphabet
    (unless (slot-boundp m 'alphabet)
      (set-alphabet (loop for i below n collecting i) m))
    (set-alphabet (coerce (alphabet m) 'vector) m)

    ;; Set next state dists
    (setf (slot-value m 'next-state-dists) (make-array n))
    (dotimes (i n)
      (setf (aref (next-state-dists m) i)
	    (make-next-state-dist m i)))))

(defun make-next-state-dist (m i)
  (with-readers (q alphabet) m
    (normalize-alist!
     (loop
       for s across alphabet
       for j from 0
       unless (= i j)
       collect (cons s (aref q i j))))))
           

(defun state-index (m s)
  (check-not-null (position s (alphabet m) :test (test m))))

(defun sample-jump (m s)
  "Given current state S, return time to next jump and new state."
  (let ((i (state-index m s)))
    (values (sample-exponential (- (aref (q m) i i)))
	    (sample (aref (next-state-dists m) i)))))
    
  
(defun sample-future-state (m s d)
  "Given state at some time T, sample the state at time T + D"
  (declare (type continuous-time-markov-chain m) (float d))
  (let ((time 0.0))
    (loop
      (mvbind (jump-time new-state) (sample-jump m s)
	(incf time jump-time)
	(if (> time d)
	    (return s)
	    (setq s new-state))))))