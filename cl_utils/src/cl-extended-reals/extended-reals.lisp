(in-package :extended-reals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype extended-real ()
  "An extended real number is either a real number, or one of the symbols 'infinity or '-infinity in the extended-reals package."
  `(or (member infinity -infinity) real))

(deftype extended-float ()
  "An extended float is either a float or one of the symbols 'infinity or '-infinity in the extended reals package."
  `(or (member infinity -infinity) float))

(define-condition extended-real-arithmetic-error () ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun e+-pairwise (x y)
  (declare (type extended-real x y))
  (the extended-real
    (cond ((realp x) (if (realp y) (+ x y) y))
	  ((and (symbolp y) (not (eql x y)))
	   (error 'extended-real-arithmetic-error))
	  (t x))))
	   
(defun e+ (&rest args)
  "Extended real addition."
  (the extended-real
  (cond
    ((null args) 0.0)
    ((null (rest args)) (first args))
    (t (destructuring-bind (f s . r) args
	 (apply #'e+ (e+-pairwise f s) r))))))

(defun negate (x)
  (declare (type extended-real x))
  (the extended-real
    (case x
      (infinity '-infinity)
      (-infinity 'infinity)
      (otherwise (- x)))))

(defun e- (x &rest args)
  "Like -, with extended reals: with single argument, negates, otherwise subtracts."
  (the extended-real
  (if args
      (apply #'e+ x (mapcar #'negate args))
      (e- 0 x))))


(defun e*-pairwise (x y)
  (declare (type extended-real x y))
  (the extended-real
    (cond 
      ((realp x) (if (realp y)
		       (* x y)
		       (cond
			 ((> x 0) y)
			 ((< x 0) (negate y))
			 (t (error 'extended-real-arithmetic-error)))))
      ((e> y 0) x)
      ((e< y 0) (negate x))
      (t (error 'extended-real-arithmetic-error)))))
  

(defun e* (&rest args)
  "Extended real multiplication"
  (the extended-real
    (cond
      ((null args) 1)
      ((null (rest args)) (first args))
      (t (destructuring-bind (f s . r) args
	   (apply #'e* (e*-pairwise f s) r))))))
      
(defun reciprocate (x)
  (declare (type extended-real x))
  (the extended-real 
    (cond
      ((zerop x) 'infinity)
      ((realp x) (/ 1 x))
      (t 0.0))))

(defun e/ (x &rest args)
  "Extended real division.  With one argument, takes reciprocal, else repeatedly divides."
  (declare (type extended-real x))
  (the extended-real
  (if args
      (apply #'e* x (mapcar #'reciprocate args))
      (e/ 1 x))))

(defun emax (&rest args)
  "Maximum of zero or more extended reals"
  (the extended-real
    (cond
      ((null args) '-infinity)
      ((null (rest args)) (first args))
      (t (destructuring-bind (f s . r) args
	   (apply #'emax (pairwise-max f s) r))))))

(defun pairwise-max (x y)
  (declare (type extended-real x y))
  (the extended-real
    (if (realp x)
	(case y
	  (infinity 'infinity)
	  (-infinity x)
	  (t (max x y)))
	(ecase x
	  (infinity 'infinity)
	  (-infinity y)))))

(defun emin (&rest args)
  "Minimum of zero or more extended reals"
  (the extended-real
    (negate
     (apply #'emax (mapcar #'negate args)))))

(defun e> (x y)
  (declare (type extended-real x y))
  (cond 
    ((or (eql x 'infinity) (eql y '-infinity)) (not (eql x y)))
    ((or (eql x '-infinity) (eql y 'infinity)) nil)
    (t (> x y))))

(defun e< (x y)
  (declare (type extended-real x y))
  (e> y x))

(defun e>= (x y)
  (declare (type extended-real x y))
  (or (e> x y) (eql x y)))

(defun e<= (x y)
  (declare (type extended-real x y))
  (or (e< x y) (eql x y)))


(defun argmax (seq &key key)
  "Return the element of the sequence that maximizes the key value (which must be an extended real), and its position, and the max key value."
  (unless key (setq key #'identity))
  (let ((dummy (gensym)))
    (let ((max-value '-infinity) (i -1) (maximizer dummy) max-pos)
      (map nil #'(lambda (x)
		   (incf i)
		   (let ((k (funcall key x)))
		     (when (e>= k max-value)
		       (setq maximizer x max-value k max-pos i))))
	   seq)
      (assert (not (eql maximizer dummy)) nil "Can't find argmax of empty sequence")
      (values maximizer max-pos max-value))))

(defun argmin (seq &key key)
  (argmax seq :key #'(lambda (x) (negate (funcall key x)))))
    