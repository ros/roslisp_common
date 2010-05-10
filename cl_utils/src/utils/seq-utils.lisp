(in-package :cl-utils)

(defun blocks (seq &key test)
  "Return list of blocks of equal (according to test) elements in sequence.  Each block is described by a pair (ELT . NUM-OCCURRENCES)."
  (orf test #'equal)
  (nreverse
   (let ((blocks nil)
	 (current-pair nil))
     (map nil
	  #'(lambda (x)
	      (if (and current-pair (funcall test x (car current-pair)))
		  (incf (cdr current-pair))
		  (progn 
		    (setq current-pair (cons x 1))
		    (push current-pair blocks))))
	  seq)
     blocks)))
	 




  
(defun is-prefix (s1 s2 &key test)
  (orf test #'eql)
  (setq s1 (coerce s1 'list)
	s2 (coerce s2 'list))
  (or (null s1)
      (and (not (null s2))
	   (dsbind (f . r) s1
	     (dsbind (f2 . r2) s2
	       (and (funcall test f f2)
		    (is-prefix r r2)))))))



(defgeneric slast (s)
  (:documentation "Return the last item of a sequence.")
  (:method ((s list)) (car (last s)))
  (:method ((s vector)) (aref s (1- (length s)))))

(defun groups-of-size (seq n &key allow-incomplete)
  "Return a list of subsequences of size N.  Setting ALLOW-INCOMPLETE true means the last block may have size < N."
  ;; Inefficient for lists
  (let ((m (length seq)))
    (mvbind (q r) (floor m n)
      (unless allow-incomplete 
	(assert (zerop r) nil "Can't make evenly ~a-sized groups from sequence ~a of length ~a" n seq m))
      (loop
	for i below q
	collect (subseq seq (* n i) (min (* n (1+ i)) m))))))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tokenize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun separator-checker (separators test)
  #'(lambda (x) (position x separators :test test)))

(defun separator-blocks (seq separators test)
  (let* ((blocks (blocks (map 'list (separator-checker separators test) seq)))
	 (m (length blocks))
	 (padded (if (oddp m) (nconc blocks `((t . 0))) blocks)))
      (groups-of-size padded 2)))
    

(defun remove-initial-separators (seq sep test) 
  (subseq seq (position-if-not (separator-checker sep test) seq)))


(defun tokenize (seq separators &key test)
  "Divide sequence into blocks separated by separators, and return the list of such blocks.  TEST is used to check if an element is a separator and defaults to #'eql.  SEPARATORS is a string or list of separators."
  (orf test #'eql)
  (setq seq (remove-initial-separators seq separators test))
  (loop
    with pos = 0
    for block in (separator-blocks seq separators test)
    for i = (cdar block)
    for j = (cdadr block)
    collect (subseq seq pos (+ pos i))
    do (setq pos (+ pos i j))))