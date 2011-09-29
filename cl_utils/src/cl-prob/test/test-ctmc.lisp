(defpackage :test-ctmc
    (:use :cl :cl-test :ctmc :cl-utils :cl-probability))

(in-package :test-ctmc)

(defparameter q #2A((-4.0 1.5 2.5) (0.1 -.5 0.4) (1.6 2.8 -4.4)))
(defparameter d #(.5 .3 .2))
(defparameter a '(a b c))
(defparameter m (make-instance 'continuous-time-markov-chain :q q :alphabet a :init-dist d))
(defparameter m2 (make-instance 'continuous-time-markov-chain :q q :init-dist d))
(defparameter q3 #2A((-1.0 .5 .5) (0.0 -1.0 1.0) (.001 0.0 -.001)))
(defparameter m3 (make-instance 'continuous-time-markov-chain :q q3 :alphabet a))

(defparameter h (loop repeat 10000 collect (sample-jump m 'a)))
(let ((*tol* .1))
  (check-equal (mean h) .25 #'close-to)
  (check-equal (std h) .25 #'close-to))

(check-randomized 
 100000 .1
 (nth-value 1 (sample-jump m2 1))
 '((0 . .2) (2 . .8)))

