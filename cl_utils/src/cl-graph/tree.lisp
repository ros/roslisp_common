(in-package :cl-graph)

;; Trees are implemented as a particular kind of graph.  Edges go from parent to child.

(defun is-tree (g)
  "Return true iff G is a tree, i.e.

1) It has a unique root node that has no parent (incoming edge)
2) Each other node has exactly one incoming edge
3) There are no cycles

Takes O(ev^2) time"

  (let ((root nil))
    (dolist (n (node-list g))
      (let ((l (length (incoming-edges g n :result-type 'list))))
	(if (= 0 l)
	    (if root
		(return-from is-tree (values nil `(multiple-roots ,root ,n)))
		(setq root n))
	    (unless (= l 1)
	      (return-from is-tree (values nil `(multiple-parents ,n)))))))
    
    (dolist (n (node-list g) t)
      (unless (eq n root)
	;; follow parents
	(let ((par (parent g n)))
	  (loop
	     (cond
	       ((eq par n) (return-from is-tree (values nil `(cycle ,n))))
	       ((eq par root) (return))
	       (t (setq par (parent g par))))))))))
		   
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjacency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
(defun parent (g n)
  "Return the unique parent node of N or signal an error.  Memoizes the parent edge in the node data."
  (declare (type graph g) (id n))
  (tail g (parent-edge g n)))

(defun parent-edge (g n)
  "Return the unique parent edge of N or signal an error.  Memoizes result in the node data."
  (declare (type graph g) (id n))
  (memoize-node-data 
   g n :parent-edge
   (let ((incoming (incoming-edges g n :result-type 'list)))
     (cond
       ((rest incoming) (error "~a has multiple parent edges ~a" n incoming))
       ((null incoming) (error "~a has no parent" n))
       (t (first incoming))))))



(defun children (g n &key (result-type 'list))
  (declare (type graph g) (id n))
  (map-iterator result-type
		(compose #'to (partial #'edge-info g))
		(outgoing-edges g n :result-type 'iterator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-child (g n &key data edge-data)
  "Returns 1) id of the new node 2) id of the edge to it"
  (declare (type graph g) (id n))
  (let ((i (add-node g :data data)))
    (values i (add-edge g n i :data edge-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-root (g n)
  (declare (type graph g) (id n))
  (let ((l (incoming-edges g n :result-type 'iterator)))
    (not (funcall l))))

(defun is-leaf (g n)
  (declare (type graph g) (id n))
  (let ((l (outgoing-edges g n :result-type 'iterator)))
    (not (funcall l))))

(defun root (g)
  (declare (type graph g))
  (check-not-null
   (find-if (partial #'is-root g) (node-list g))))

(defun path-from-root (g n)
  (declare (type graph g) (id n))
  (labels ((helper (g n l)
	     (if (is-root g n)
		 (cons n l)
		 (helper g (parent g n) (cons n l)))))
    (helper g n nil)))

(defun depth (g n)
  (declare (type graph g) (id n))
  (labels ((helper (g n d)
	     (if (is-root g n)
		 d
		 (helper g (parent g n) (1+ d)))))
    (helper g n 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tree*)
(defvar *current*)
(defvar *current-edge*)

(defun inspect-tree (tree &optional (n 0))
  (setq *tree* tree
	*current* n)
  (update-current-edge)
  (print-local-tree))

(defun update-current-edge ()
  (setq *current-edge* (if (is-root *tree* *current*) nil (parent-edge *tree* *current*))))

(defun up (&optional (n 1))
  (dotimes (i n)
    (setq *current* (parent *tree* *current*)))
  (update-current-edge)
  (print-local-tree))

(defun down (i)
  (assert (member i (outgoing-edges *tree* *current*)))
  (setq *current* (head *tree* i))
  (update-current-edge)
  (print-local-tree))

	  

(defun print-local-tree ()
  (let ((str t))
    (pprint-logical-block (str nil :prefix "[" :suffix "]")
      (format str "Node ~a~:@_ ~a" *current* (get-node-data *tree* *current*))
      
      (format str "~:[~;~:*~:@_Parent edge ~a~:@_ ~a~]" *current-edge* (when *current-edge* (get-edge-data *tree* *current-edge*)))
      (dolist (e (outgoing-edges *tree* *current*))
	(format str "~:@_Child edge ~a~:@_ ~a" e (get-edge-data *tree* e))))))
      