(in-package :cl-prob)

;; Discrete probability distributions represented as alist, where entries are compared using #'equal

(defmethod probability ((dist list) event)
  (let ((fn (event-function event)))
    (reduce #'+ dist :key #'(lambda (pair)
			      (dsbind (item . prob) pair
				(declare ((or float rational) prob))
				(if (funcall fn item) prob 0.0))))))

(defmethod expectation ((dist list) (rv function))
  (reduce #'+ dist :key #'(lambda (entry) (* (funcall rv (car entry)) (cdr entry)))))


(defmethod condition-on-event ((dist list) event &key return-type)
  ;; Todo put in declarations
  (assert (null return-type))
  (let* ((fn (event-function event))
	 (norm (probability dist fn)))
    (assert (> norm 0) nil "Can't condition dist ~a on event ~a with probability 0" dist event)
    (loop
       for pair in dist
       for x = (car pair)
       when (funcall fn x)
       collect (cons x (/ (cdr pair) norm)))))

(defmethod sample ((dist list))
  (let ((p (random 1.0))
	(s 0.0))
    (dolist (pair dist (progn (warn "Unexpectedly reached end with s=~a, p=~a when sampling from ~a; using last element" s p dist)
			      (caar (last dist))))
      (dsbind (item . prob) pair
	(declare (float prob))
	(incf s prob)
	(when (> s p)
	  (return item))))))

(defun normalize-alist! (dist)
  "Normalize an alist distribution to sum to 1"
  (let ((total (reduce #'+ dist :key #'cdr)))
    (assert (not (zerop total)))
    (dolist (pair dist dist)
      (_f / (cdr pair) total))))
  

