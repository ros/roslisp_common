(in-package :cl-prob)

;; Discrete probability distributions represented as vectors of floats
;; #(0.3 0.5 0.2) assigns probability .3 to 0, 0.5 to 1, and 0.2 to 2

(deftype vector-dist (&optional n)
  "Vector representing probability distribution over 0,1,...,n-1; n may be left out"
  `(vector (or rational float) ,n))

(defmethod probability ((dist vector) event)
  (declare (type vector-dist dist))
  (loop
    with fn = (event-function event)
    for i from 0
    for x across dist
    when (funcall fn i) 
      sum x))

(defmethod expectation ((dist vector) (rv function))
  (declare (type vector-dist dist))
  (loop
    for i from 0
    for x across dist
    sum (* x (funcall rv i))))

(defmethod sample ((dist vector))
  (loop
    with p = (random 1.0)
    with s = 0.0
    for i from 0
    for x across dist
    do (incf s x)
    when (> s p)
      return i
    finally (warn "Reached end with p = ~a when sampling from ~a" p dist)
	    (return (1- (length dist)))))
      
    