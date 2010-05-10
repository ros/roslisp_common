(in-package :cl-utils)

(deftype non-null ()
  '(not null))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defvars (&body var-names)
  (cons 'progn (mapcar #'(lambda (name) `(defvar ,name)) var-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-symbol-name (&rest args)
  "build-symbol-name S1 ... Sn.  Each Si is a string or symbol.  Concatenate them into a single long string (which can then be turned into a symbol using intern or find-symbol."
  (apply #'concatenate 'string (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) args)))

(defun intern-compound-symbol (&rest args)
  "intern-compound-symbol S1 ... Sn.  Interns the result of build-symbol-name applied to the S."
  (intern (apply #'build-symbol-name args)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms (syms &body body)
  "Bind the value of each symbol in SYMS to a unique gensym, then evaluate body"
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
	  syms)
     ,@body))

(defmacro with-readers (reader-specs object &body body)
  "with-readers READER-SPECS OBJECT &body BODY

READER-SPECS (unevaluated) is a list of specs, each of which is either of the form (VAR FN) (symbols) or just VAR (which is then treated as (VAR VAR)).
OBJECT is any object (evaluated once).

Set up bindings for the variables as functions of the given object, then evaluate the body."
  (let ((vars (mapcar #'(lambda (spec) (if (listp spec) (first spec) spec)) reader-specs))
	(fnames (mapcar #'(lambda (spec) (if (listp spec) (second spec) spec)) reader-specs))
	(obj (gensym)))
    `(let ((,obj ,object))
       (let ,(mapcar #'(lambda (v f) `(,v (,f ,obj))) vars fnames)
	 ,@body))))

(defmacro with-struct ((name &rest fields) s &body body)
  "with-struct (CONC-NAME . FIELDS) S &rest BODY

"

  (let ((gs (gensym)))
    `(let ((,gs ,s))
       (symbol-macrolet 
	   ,(mapcar #'(lambda (f)
			`(,f (,(intern-compound-symbol name f) ,gs)))
	      fields)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; condlet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro condlet (clauses &body body)
  "condlet CLAUSES &rest BODY.  CLAUSES is a list of which each member is a conditional binding or otherwise clause.  There can be at most one otherwise clause and it must be the last clause.  Each conditional binding is a list where the first element is a test and the remaining elements are the bindings to be made if the test succeeds.   Each clause must bind the same set of variables.  If one of the tests succeeds, the corresponding bindings are made, and the body evaluated.  If none of the tests suceeds, the otherwise clause, if any, is evaluated instead of the body."
  (labels ((condlet-clause (vars cl bodfn)
	     `(,(car cl) (let ,(condlet-binds vars cl)
			   (,bodfn ,@(mapcar #'cdr vars)))))
	   
	   (condlet-binds (vars cl)
	     (mapcar #'(lambda (bindform)
			 (if (consp bindform)
			     (cons (cdr (assoc (car bindform) vars))
				   (cdr bindform))))
		     (cdr cl))))
	   
    (let* ((var-names (mapcar #'car (cdr (first clauses))))
	   (otherwise-clause? (eql (caar (last clauses)) 'otherwise))
	   (actual-clauses (if otherwise-clause? (butlast clauses) clauses)))
      (assert (every (lambda (cl) (equal var-names (mapcar #'car (cdr cl))))
		     actual-clauses)
	  nil "All non-otherwise-clauses in condlet must have same variables.")
      (let ((bodfn (gensym))
	    (vars (mapcar (lambda (v) (cons v (gensym)))
			  var-names)))
	`(labels ((,bodfn ,(mapcar #'car vars)
		    ,@body))
	   (cond 
	    ,@(mapcar (lambda (cl) (condlet-clause vars cl bodfn))
		      actual-clauses)
	    ,@(when otherwise-clause? `((t (progn ,@(cdar (last clauses))))))))))))


       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro verify-type (x typespec &optional (msg (format nil "~a" typespec)))
  "verify-type X TYPESPEC &optional MSG.   Shorthand for check-type X typespec msg followed by returning x."
  (let ((y (gensym)))
    `(let ((,y ,x))
       (check-type ,y ,typespec ,msg)
       ,y)))

(defmacro check-not-null (x &rest args)
  (let ((y (gensym)))
    `(let ((,y ,x))
       (if ,y 
	   ,y
	   ,(if args
		`(error ,@args)
		`(assert nil nil "~a was unexpectedly nil" ',x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructuring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
    `(let ,(mapcar #'(lambda (b)
		       (if (consp (car b))
			   (car b)
			 b))
	    binds)
       ,(dbind-ex (mapcan #'(lambda (b)
			      (if (consp (car b))
				  (cdr b)))
			  binds)
		  body))))


(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (labels ((true-list (x)
	     (or (null x)
		 (and (consp x)
		      (true-list (cdr x))))))
    (assert (true-list pat))
    (if (null pat)
	nil
      (let ((rest (cond ((funcall atom? pat) pat)
			((eq (car pat) '&rest) (cadr pat))
			((eq (car pat) '&body) (cadr pat))
			(t nil))))
	(if rest
	    `((,rest (subseq ,seq ,n)))
	  (let ((p (car pat))
		(rec (destruc (cdr pat) seq atom? (1+ n))))
	    (if (funcall atom? p)
		(cons `(,p (elt ,seq ,n)) rec)
	      (let ((var (gensym)))
		(cons (cons `(,var (elt ,seq ,n))
			    (destruc p var atom?))
		      rec)))))))))


(defmacro dbind (pat seq &body body)
  "macro dbind PAT SEQ BODY.  Like destructuring-bind except SEQ may now be any combination of lists and vectors.  The format of PAT is as before regardless.  See also dsbind."
    (let ((gseq (gensym)))
      `(let ((,gseq ,seq))
	 ,(dbind-ex (destruc pat gseq #'atom) body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro while (test &rest body)
  "while TEST &rest BODY
A looping construct.  TEST is evaluated *before* each iteration, and if it is false, the construct terminates."
  `(loop
       while ,test
       do ,@ (or body '(()))))

(defmacro till (test &body body)
  "till TEST &rest BODY
Looping construct equivalent to while (not TEST) . BODY.  See also repeat-until."
  `(while (not ,test) ,@body))

(defmacro repeat-until (test &rest body)
  "repeat-until TEST &rest BODY
Looping construct.  TEST is evaluated *after* each iteration (unlike while and until), and if it is true, the construct terminates."
  `(loop
     ,@body
     (when ,test (return))))


(defmacro repeat (n &body body)
  "repeat N &rest BODY.  Execute BODY N times."
  (let ((i (gensym)))
    `(dotimes (,i ,n)
       (declare (ignorable ,i))
       ,@body)))

(defmacro for-loop ((var i j &optional (inc 1) (test '#'>=)) &body body)
  "for (VAR I J &optional (INC 1) (TEST #'>=)) &body BODY.

Repeatedly executes BODY with VAR bound to I,I+INC,...  The loop is stopped before executing the body when TEST returns true given VAR and J."
  (with-gensyms (last increment)
    `(let ((,last ,j)
	   (,increment ,inc))
       (do ((,var ,i (incf ,var ,increment)))
	   ((funcall ,test ,var ,last) nil)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbreviations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro abbrev (x y)
  (let ((args (gensym))
	(doc-string (format nil "~a : Abbreviation macro for ~a" x y)))

    `(defmacro ,x (&rest ,args)
       ,doc-string
       `(,',y ,@,args))))

(abbrev mvsetq multiple-value-setq)
(abbrev unbind-slot slot-makunbound)

(defmacro unbind-slots (o &rest args)
  "unbind-slots OBJ SLOT1 ... SLOTk
slot names are unevaluated (no quote needed)."
  (with-gensyms (obj)
    `(let ((,obj ,o))
       ,@(mapcar #'(lambda (a) `(unbind-slot ,obj ',a)) args))))

;; Spell these out so slime can indent correctly
(defmacro mvbind (vars form &body body)
  "abbreviation for multiple-value-bind"
  `(multiple-value-bind ,vars ,form ,@body))

(defmacro dsbind (pattern form &body body)
  "abbreviation for destructuring-bind"
  `(destructuring-bind ,pattern ,form ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-symmetric-method (name (a1 a2 &rest remaining) &body body)
  "def-symmetric-method NAME (ARG1 ARG2 &rest REMAINING) &rest BODY

Expands to code that defines two methods.  The first is obtained by replacing def-symmetric-method with defmethod.  The second is obtained by reversing the first two args"
  `(progn
     (defmethod ,name (,a1 ,a2 ,@remaining) ,@body)
     (defmethod ,name (,a2 ,a1 ,@remaining) ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anaphoric macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setf macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro _f (op place &rest args)
  "_f OP PLACE &rest ARGS.  A correct (i.e. multiple-evaluation-avoiding) version of (setf PLACE (apply OP PLACE ARGS))"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (, (car var) (,op ,access ,@args)))
       ,set)))

(defmacro orf (x &rest args)
  `(_f or ,x ,@args))

(defmacro toggle (place)
  `(_f not ,place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Randomized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-histogram (hist item &key test)
  (orf test #'equal)
  (let ((pair (assoc item hist :test test)))
    (if pair
	(progn (incf (cdr pair)) hist)
	(cons (cons item 1) hist))))

(defun normalize-histogram (hist)
  (let ((total (reduce #'+ hist :key #'cdr)))
    (mapcar #'(lambda (pair)
		(dsbind (key . count) pair
		  (cons key (float (/ count total)))))
	    hist)))

(defmacro generate-histogram (form num-trials &key test normalize)
  "Execute the (presumably randomized) FORM NUM-TRIALS times and return a histogram of the resulting values.  TEST is used to determine when two values are the same."
  (orf test #'equal)
  (with-gensyms (hist)
    `(let ((,hist nil))
       (repeat ,num-trials
	 (_f update-histogram ,hist ,form :test ,test))
       (if ,normalize (normalize-histogram ,hist) ,hist))))

(defun hist-count (hist pred)
  (reduce #'+ hist :key #'(lambda (pair) (if (funcall pred (car pair)) (cdr pair) 0))))

(defmacro if-rand (p f1 f2)
  "With probability P do F1, else F2."
  `(if (< (random 1.0) ,p) ,f1 ,f2))

(defmacro when-rand (p &body f1)
  "With probability P do F1, else nil."
  `(when (< (random 1.0) ,p) ,@f1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro bind-pprint-args ((str obj) args &body body)
  "bind-pprint-args (STR OBJ) ARGS &rest BODY

STR, OBJ : unevaluated symbols
ARGS : a list (evaluated)


If ARGS has length 1, bind STR to t, OBJ to (FIRST ARGS).  Otherwise, bind STR to (first args) and OBJ to (second args) ARGS.  Then evaluate BDOY in this lexical context."
  
  (let ((x (gensym)))
    `(let ((,x ,args))
       (condlet
	(((= 1 (length ,x)) (,str t) (,obj (first ,x)))
	 (t (,str (first ,x)) (,obj (second ,x))))
	,@body))))



