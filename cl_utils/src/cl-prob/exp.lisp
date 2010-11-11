;; Exponential distribution

(in-package :cl-prob)


(defun sample-exponential (r)
  "Sample from an exponential distribution with rate r (mean 1/r)"
  (declare (float r))
  (/ (log (- 1.0 (random 1.0))) (- r)))
  

