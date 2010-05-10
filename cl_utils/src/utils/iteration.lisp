(in-package :cl-utils)

(defgeneric iter (x)
  (:documentation "return iterator over X"))

(defmethod iter ((x list))
  (let ((l x))
    #'(lambda ()
	(when l
	  (values t (pop l))))))

	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code to do with lazy sequences represented by an 
;; iterator: a function of no arguments that repeatedly
;; returns elements of the sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-iterator ((vars i &optional ret-val) &body body)
  "I must be a function of zero arguments, where the first return value is nil iff the iteration is complete and the remaining values are the actual elements in this round of the iteration.  So long as the iteration is not finished, VARS are bound to those values and the BODY is repeatedly evaluated."
  (if (symbolp vars) (setq vars (list vars)))
  (with-gensyms (not-done iter)
    `(let ((,iter ,i))
       (loop
	  (mvbind ,(cons not-done vars) (funcall ,iter)
	    (if ,not-done
		(progn ,@body)
		(return ,ret-val)))))))



(defun map-iterator (&rest args)
  "map-iterator [RESULT-TYPE] FUNCTION ITER

RESULT-TYPE (default value is 'iterator) is assumed given if the first argument is a symbol.  It can be 'iterator, 'list, or 'vector (the last two from the :cl package)."

  (condlet
      (((symbolp (first args)) (result-type (first args)) (fn (second args)) (iter (third args)))
       (t (result-type 'iterator) (fn (first args)) (iter (second args))))
    (ecase result-type
      (iterator #'(lambda ()
		    (mvbind (not-done x) (funcall iter)
		      (when not-done
			(values t (funcall fn x))))))
      (list (let ((l nil))
	      (do-iterator (x iter (nreverse l))
		(push (funcall fn x) l))))
      (vector (let ((v (make-adjustable-vector)))
		(do-iterator (x iter v)
		  (vector-push-extend (funcall fn x) v)))))))



(defun realize (iter)
  "Realize an iterator into a list"
  (map-iterator 'list #'identity iter))


(defun take (n iter)
  "Take at most N elements from ITER."
  (let ((i 0))
    #'(lambda ()
	(when (<= (incf i) n)
	  (funcall iter)))))

(defun range (&key (from 0) (by 1) below above)
  "Return iterator over arithmetic progression of integers"
  (let ((i from))
    #'(lambda ()
	(unless (or (and below (>= i below))
		    (and above (<= i above)))
	  (values t (prog1 i (incf i by)))))))

(defun take-until (fn iter &key include-last)
  "Take elements until FN becomes true.  INCLUDE-LAST true means include the first element that makes FN true; otherwise, we stop right before it.  Only works for iterators returning a single value."
  (let ((stop nil))
    #'(lambda ()
	(unless stop
	  (mvbind 
	      (not-done x) (funcall iter)
	    (when not-done
	      (if (funcall fn x)
		  (when include-last
		    (setq stop t)
		    (values t x))
		  (values t x))))))))

(defun concat (&rest seqs)
  "Return concatenation of a bunch of sequences."
  (dsbind (f . r) seqs
    #'(lambda ()
	(let (x not-done)
	  (loop
	     (mvsetq (not-done x) (funcall f))
	     (if not-done
		 (return (values t x))
		 (if r
		     (setq f (pop r))
		     (return))))))))

(defun elements-satisfying (f iter)
  "Return new iterator over elements that satisfy F"
  #'(lambda ()
      (loop
	 (mvbind (not-done val) (funcall iter)
	   (if not-done
	       (when (funcall f val)
		 (return (values t val)))
	       (return nil))))))

		     
	     
	
		   