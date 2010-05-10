;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function-utils.lisp
;; general utilities for functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-utils)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional compose
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compose (&rest functions)
  "compose F1 F2 ... Fn
For now, all the Fi except the last one must be unary.  Return F s.t. (apply f #'args) = (funcall f1 (funcall f2 ... (apply fn args)))"
  (assert functions)
  (let ((functions (reverse functions)))
    (dsbind (f1 . frest) functions
      #'(lambda (&rest args)
	  (let ((x (apply f1 args)))
	    (dolist (f frest x)
	      (setf x (funcall f x))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pick out nth arg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro nth-arg-fn (n)
  "nth-arg-fn N.  N (unevaluated) must be a positive integer.  Expand to code that evaluates to a function that returns its Nth argument, ignoring the others."
  (let ((vars (loop for i upto n collecting (gensym)))
	(args (gensym)))
    `#'(lambda (,@vars &rest ,args)
	 (declare (ignore ,args ,@(butlast vars)))
	 ,(nth n vars))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building functions from other functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO correct in compose case

(defmacro fn (expr &optional (num-args 1) )
  "Macro FN EXPR &optional (NUM-ARGS 1).  Build up complex functions from expressions.  E.g. (fn (and integerp oddp)) expands to a function that returns true iff its argument is an odd integer.  Taken from Paul Graham's On Lisp.  See the description at the beginning of Chapter 15 for more details.  Have added the optional argument NUM-ARGS.  The idea is to be able to write something like (fn (* + -) 2) to return the equivalent of (lambda (x y) (* (+ x y) (- x y))).  It works in simple cases right now, but is unlikely to interact well with 'compose."
  `#',(rbuild expr num-args))

(defun rbuild (expr num-args)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
    (if (eq (car expr) 'compose)
	(build-compose (cdr expr))
      (build-call (car expr) (cdr expr) num-args))))

(defun build-call (op fns num-args)
  (let ((ga nil))
    (dotimes (i num-args)
      (push (gensym) ga))
    `(lambda ,ga
       (,op ,@(mapcar #'(lambda (f) `(,(rbuild f num-args) ,@ga))
		      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		   (if fns
		       `(,(rbuild (car fns) 1)
			 ,(rec (cdr fns)))
		     g)))
	  (rec fns)))))

(defun arglist-fn (f)
  #'(lambda (&rest args) (funcall f args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aggregation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defaggregator (fn-name binary-fn-name default-value-form)
  "Define a function FN-NAME that takes a variable number of arguments.  If given 0 arguments, return value of default-value-form.  If given 1 argument, return that argument.  Else, apply binary-fn-name to first argument and recursive value of FN-NAME on remaining arguments.  Returned function is suitable as an argument to reduce."
  (let ((args (gensym)))
    `(defun ,fn-name (&rest ,args)
       (if ,args
	   (,binary-fn-name (car ,args) (apply #',fn-name (cdr ,args)))
	   ,default-value-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun partial (fn &rest some-args)
  "Return fn with first k args bound to some-args"
  #'(lambda (&rest remaining-args) (apply fn (append some-args remaining-args))))

(defun rcurry (fn &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lambda (&rest more)
    (declare (dynamic-extent more))
    (multiple-value-call fn (values-list more) (values-list arguments))))