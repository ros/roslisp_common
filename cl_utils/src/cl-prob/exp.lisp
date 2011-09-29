;; Exponential distribution

(in-package :cl-prob)


(defun sample-exponential (r)
  "Sample from an exponential distribution with rate r (mean 1/r)"
  (declare (float r))
  (/ (log (- 1.0 (random 1.0))) (- r)))

(defun exponential-cdf (r x)
  (declare (float r x))
  (- 1 (exp (- (* r x)))))
  

(defclass exponential ()
  ((rate :initarg :rate :type float :reader rate)))

(defmethod sample ((d exponential))
  (sample-exponential (rate d)))

(defmethod probability ((d exponential) (event interval))
  (let ((r (rate d)))
    (with-readers (left right) event
      (- (exponential-cdf r right) (exponential-cdf r left)))))