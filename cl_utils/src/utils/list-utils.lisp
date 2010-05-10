(in-package :cl-utils)

(defun lookup-alist (k l &rest args)
  "lookup-alist K L [DEFAULT] &rest ARGS.  Get value corresponding to K in L.  Signal error if K not present and DEFAULT not specified.  Remaining args passed on to assoc.  They must be keyword pairs, which allows determining whether or not DEFAULT has been specified."
  (if (oddp (length args))
      (let ((pair (apply #'assoc k l (cdr args))))
	(if pair (cdr pair) (car args)))
      (cdr (check-not-null (apply #'assoc k l args)))))

(defun filter (pred l)
  "Return list of elements in L that satisfy PRED"
  (loop for x in l
     when (funcall pred x) collect x))

(defun remove-nth (n l)
  (nconc (subseq l 0 n) (nthcdr (1+ n) l)))

(defun is-permutation (l1 l2 &key test)
  (orf test #'eql)
  (and (= (length l1) (length l2))
       (every #'(lambda (x) (member x l2 :test test)) l1)))


(defun length-equals (k l)
  "Determine whether L's length equals K in O(K) time (as opposed to O(|L|))."
  (cond
    ((zerop k) (null l))
    ((null l) (zerop k))
    (t (length-equals (1- k) (cdr l)))))