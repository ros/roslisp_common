(defpackage :test-graph
  (:use :cl :cl-test :cl-graph :cl-utils :extended-reals))

(in-package :test-graph)

(defvars *graph* *n1* *n2* *n3* *n4* *e1* *e2* *e3*)

;; Create empty graph
(setq *graph* (make-graph))
(check-equal (node-list *graph*) nil)
(check-error (neighbors *graph* 1))
(check-error (adjacent-edge-list *graph* 1))
(check-equal (edge-list *graph*) nil)


;; Add nodes
(setq *n1* (add-node *graph* :id 'foo)
      *n2* (add-node *graph*)
      *n3* (add-node *graph* :id 'bar))

(check-equal (node-list *graph*) (list *n1* *n2* *n3*) #'is-permutation)
(check-equal (edge-list *graph*) nil)
(check-equal (neighbors *graph* *n1*) nil)
(check-equal (adjacent-edge-list *graph* *n2*) nil)


;; Add edges
(setq *e1* (add-edge *graph* *n3* *n1*)
      *e2* (add-edge *graph* *n1* *n3* :id 'baz)
      *e3* (add-edge *graph* *n2* *n3*))

(check-equal (node-list *graph*) (list *n1* *n2* *n3*) #'is-permutation)
(check-equal (edge-list *graph*) (list *e1* *e2* *e3*) #'is-permutation)
(check-equal (adjacent-edge-list *graph* *n1*) (list *e1* *e2*) #'is-permutation)
(check-equal (neighbors *graph* *n1*) (list *n3*))
(check-true (member (edge-between *graph* *n1* *n3*) (list *e1* *e2*)))
(check-true (member (edge-between *graph* *n3* *n1*) (list *e1* *e2*)))
(check-equal (edge-between *graph* *n1* *n2*) nil)
(check-equal (edge-between *graph* *n2* *n1*) nil)

;; Test make-undirected graph
(defvar g)


;; Shortest paths

(defun close-pairs (p1 p2)
  (and (eql (car p1) (car p2))
       (< (abs (- (cdr p2) (cdr p1))) .1)))

(defvars links nf rt n-path e-path)
(setq links '((f (b (:length . 2)))
	      (b (a (:length . 3)))
	      (a (c (:length . 3.2)) (e (:length . 2.4)))
	      (c (d (:length . 4.1)))
	      (d (e (:length . 1.6)))
	      (e)
	      (g (h (:length . 4.4)) (i (:length . 2.1)))
	      (h (i (:length . 1.6)) (j (:length . 3.3)))
	      (i)
	      (j))

      g (make-undirected-graph links))


(mvsetq (nf rt) (compute-navfn g 'e))
(check-equal (hash-table-to-alist nf) 
	     '((e . 0.0) (d . 1.6) (a . 2.4) (c . 5.6) (b . 5.4) (f . 7.4)) #'(lambda (l1 l2) (is-permutation l1 l2 :test #'close-pairs)))

(mvsetq (e-path n-path) (extract-path g rt 'c))
(check-equal e-path (list (edge-between g 'c 'a) (edge-between g 'a 'e)))
(check-equal n-path '(c a e))

(setf (third links) '(a (c (:length . 3.2)) (e (:length . 2.8)))
      g (make-undirected-graph links))
(mvsetq (nf rt) (compute-navfn g 'e))
(check-equal (hash-table-to-alist nf) 
	     '((e . 0.0) (d . 1.6) (a . 2.8) (c . 5.7) (b . 5.8) (f . 7.8)) #'(lambda (l1 l2) (is-permutation l1 l2 :test #'close-pairs)))

(mvsetq (e-path n-path) (extract-path g rt 'c))
(check-equal e-path (list (edge-between g 'c 'd) (edge-between g 'd 'e)))
(check-equal n-path '(c d e))

      


;; Copy-graph
(defvars g2)
(setq g2 (copy-graph g))
(mvsetq (nf rt) (compute-navfn g2 'e))
(check-equal (hash-table-to-alist nf) 
	     '((e . 0.0) (d . 1.6) (a . 2.8) (c . 5.7) (b . 5.8) (f . 7.8)) #'(lambda (l1 l2) (is-permutation l1 l2 :test #'close-pairs)))

(mvsetq (e-path n-path) (extract-path g rt 'c))
(check-equal e-path (list (edge-between g2 'c 'd) (edge-between g2 'd 'e)))
(check-equal n-path '(c d e))


;; Remove edge
(check-equal (nth-value 1 (shortest-path g2 'e 'c)) '(c d e))
(remove-edge g2 (edge-between g2 'd 'e))
(check-equal (nth-value 1 (shortest-path g2 'e 'c)) '(c a e))
(check-equal (adjacent-edge-list g2 'd) (list (edge-between g2 'c 'd)))
(check-equal (neighbors g2 'd) '(c))


;; Diameter
(defvar g-diam)
(setq g-diam (make-undirected-graph
	      '((f (b (:length . 2)))
	       (b (a (:length . 3)))
	       (a (c (:length . 3.2)) (e (:length . 2.4)))
	       (c (d (:length . 4.1)))
	       (d (e (:length . 1.6)))
	       (e (g (:length . 1.0)))
	       (g (h (:length . 4.4)) (i (:length . 2.1)))
	       (h (i (:length . 1.6)) (j (:length . 3.3)))
	       (i)
	       (j))))

(let ((*tol* .001))
  (check-equal (diameter g-diam) 15.4 #'close-to))
(check-equal (diameter g) 'infinity)
  

;; Remove nodes
(defvars g3 n1 n2 n3 n4 n5 e12 e13 e14 e23 e25)
(setq g3 (make-graph)
      n1 (add-node g3)
      n2 (add-node g3)
      n3 (add-node g3)
      n4 (add-node g3)
      e12 (add-edge g3 n1 n2)
      e13 (add-edge g3 n3 n1)
      e14 (add-edge g3 n1 n4)
      e23 (add-edge g3 n2 n3))

;; Pre-removal
(check-equal (adjacent-edge-list g3 n2) (list e12 e23) #'is-permutation)
(check-equal (adjacent-edge-list g3 n1) (list e12 e13 e14) #'is-permutation)
(check-equal (adjacent-edge-list g3 n3) (list e13 e23) #'is-permutation)
(check-equal (neighbors g3 n2) (list n1 n3) #'is-permutation)

(check-error (remove-node g3 n3))
(check-equal (adjacent-edge-list g3 n2) (list e12 e23) #'is-permutation)
(check-equal (adjacent-edge-list g3 n1) (list e12 e13 e14) #'is-permutation)
(check-equal (adjacent-edge-list g3 n3) (list e13 e23) #'is-permutation)
(check-equal (neighbors g3 n2) (list n1 n3) #'is-permutation)

(remove-node g3 n3 :remove-edges t)
(check-equal (adjacent-edge-list g3 n2) (list e12) #'is-permutation)
(check-equal (adjacent-edge-list g3 n1) (list e12 e14) #'is-permutation)
(check-error (adjacent-edge-list g3 n3))
(check-equal (neighbors g3 n2) (list n1) #'is-permutation)

(setq n5 (add-node g3)
      e25 (add-edge g3 n2 n5))
(check-equal (adjacent-edge-list g3 n2) (list e12 e25) #'is-permutation)
(check-equal (adjacent-edge-list g3 n1) (list e12 e14) #'is-permutation)
(check-error (adjacent-edge-list g3 n3))
(check-equal (adjacent-edge-list g3 n5) (list e25))
(check-equal (neighbors g3 n2) (list n5 n1) #'is-permutation)


