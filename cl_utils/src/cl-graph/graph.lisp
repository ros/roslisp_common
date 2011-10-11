;; Minimal graph implementation using adjacency list
;; Edges and nodes have integer id's and can have associated data

(in-package :cl-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype id ()
  '(or symbol fixnum))

(defstruct (graph (:conc-name nil) (:constructor create-graph) (:copier nil))
  nodes edges (next-node-id 0 :type fixnum) (next-edge-id 0 :type fixnum))

(defstruct (node-info (:conc-name nil) (:constructor make-node-info (node-data adjacent-edges)) (:type list))
  node-data adjacent-edges)

(defstruct (edge-info (:conc-name nil) (:constructor make-edge-info (edge-data from to)) (:type list))
  edge-data 
  (from nil :type id) 
  (to nil :type id))

(defun make-graph ()
  (create-graph :nodes (make-hash-table :test #'eql) 
		:edges (make-hash-table :test #'eql)))

(defun copy-graph (g)
  "Return a new graph that is a copy of G.  Edge and node data and ids are shallow copied."
  (let ((g2 (make-graph)))
    (dolist (n (node-list g))
      (declare (type id n))
      (add-node g2 :data (copy-alist (get-node-data g n)) :id n))
    (dolist (e (edge-list g))
      (declare (type id e))
      (dsbind (from to) (incident-nodes g e)
	(declare (type id from to))
	(add-edge g2 from to :id e :data (copy-alist (get-edge-data g e)))))
    g2))
  


(defun convert-adj-specs-to-list (adj-specs)
  (etypecase adj-specs
    (list adj-specs)
    (hash-table 
       (let ((l nil))
	 (do-hash-entries ((v edge-specs) adj-specs l)
	   (push (cons v edge-specs) l))))))

(defun make-undirected-graph (adj-specs)
  "adj-specs is list of elements of form (V E1 ... En) where V are node names and each Ei is either a node name or a pair (NODE-NAME . EDGE-DATA).  Adj-specs can also be hash table from V to (E1 ... En) using the above notation."
  (let ((g (make-graph))
	(adj-specs (convert-adj-specs-to-list adj-specs)))
    (dolist (spec adj-specs)
      (add-node g :id (first spec)))
    (dolist (spec adj-specs g)
      (destructuring-bind (v . edge-specs) spec
	(dolist (spec edge-specs)
	  (condlet (((consp spec) (v2 (car spec)) (data (cdr spec)))
		    (t (v2 spec) (data nil)))
	    (when (edge-between g v v2)
	      (error "Trying to add duplicate edge between ~a and ~a" v v2))
	    (add-edge g v v2 :data data)))))))
      

(defun node-info (g i)
  (declare (type graph g) (type id i))
  (check-not-null (gethash i (nodes g)) "Node ~a unknown in ~a" i g))

(defun edge-info (g i)
  (declare (type graph g) (type id i))
  (check-not-null (gethash i (edges g)) "Edge ~a unknown in ~a" i g))

(defun get-edge-data (g i)
  (declare (type graph g) (type id i))
  (edge-data (edge-info g i)))

(defun get-node-data (g i)
  (declare (type graph g) (type id i))
  (node-data (node-info g i)))

(defun lookup-edge-data (g k i)
  (declare (type graph g) (type id i) (symbol k))
  (lookup-alist k (get-edge-data g i)))

(defun lookup-node-data (g k i)
  (declare (type graph g) (type id i) (symbol k))
  (lookup-alist k (get-node-data g i)))

(defun node-list (g)
  (hash-keys (nodes g)))

(defun edge-list (g)
  (hash-keys (edges g)))

(defun lookup-node (g key val)
  (check-not-null
   (dolist (id (node-list g))
     (let ((data (node-data (node-info g id))))
       (when (aand (assoc key data) (equal (cdr it) val))
	 (return id))))))

(defun head (g e)
  (declare (type graph g) (type id e))
  (to (edge-info g e)))

(defun tail (g e)
  (declare (type graph g) (type id e))
  (from (edge-info g e)))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjacency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun incident-nodes (g e)
  (declare (type graph g) (type id e))
  (with-readers (from to) (edge-info g e)
    (declare (type id from to)) ;; this sort of declaration is probably unnecessary since the structure slot type is declared
    (list from to)))

(defun other-node (g e i)
  (declare (type graph g) (type id e i))
  (let ((edge-info (edge-info g e)))
    (with-readers (from to) edge-info
      (declare (type id from to))
      (cond
	((eql from i) to)
	((eql to i) from)
	(t (error "Edge ~a was not incident to node ~a" edge-info i))))))

(defun adjacent-edge-list (g i)
  (declare (type graph g) (type id i))
  (adjacent-edges (node-info g i)))

(defun edge-between (g i j)
  "Return (the id of) an edge between I and J if one exists, or nil otherwise."
  (declare (type graph g) (type id i j))
  (dolist (e (adjacent-edge-list g i))
    (declare (type id e))
    (when (eql j (other-node g e i))
      (return-from edge-between e))))

(defun neighbors (g i)
  "Node ids of neighbors i in g, with duplicates removed"
  (declare (type graph g) (type id i))  
  (let ((l nil))
    (dolist (e (adjacent-edge-list g i) l)
      (declare (type id e))
      (pushnew (other-node g e i) l))))

(defun incoming-edges (g n &key (result-type 'list))
  (declare (type graph g) (type id n))  
  (edge-filter-helper
   #'(lambda (e) (declare (type id e)) (eql (to (edge-info g e)) n))
   (adjacent-edge-list g n) result-type))

(defun edge-filter-helper (f l result-type)
  (ecase result-type
    (list (filter f l))
    (iterator (elements-satisfying f (iter l)))))

(defun outgoing-edges (g n &key (result-type 'list))
  (declare (type graph g) (type id n))  
  (edge-filter-helper 
   #'(lambda (e) (declare (type id e)) (eql (from (edge-info g e)) n))
   (adjacent-edge-list g n) result-type))
   
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-node (g &key data id)
  "Add a node.  You can provide your own ID, so long as it's not an integer (those are reserved for the defaults).  Returns the added node's id."
  (declare (type graph g) (type id id))
  (with-readers (nodes) g
    
    ;; Either use the provided id or the next free id
    (symbol-macrolet ((next (next-node-id g)))
      (while (hash-table-has-key nodes next)
	(incf next))
      (orf id next))

    (assert (not (hash-table-has-key nodes id)))
    (setf (gethash id nodes) (make-node-info data nil))
    id))


(defun add-edge (g from to &key id data)
  "Add an edge between two nodes.  You can provide your own ID, so long as its not an integer (those are reserved for the defaults).  Returns the added edge's id."
  (declare (type graph g) (type id id from to))
  (with-readers (edges) g

    ;; Either use the provided id or the next free one
    (symbol-macrolet ((next (next-edge-id g)))
      (while (hash-table-has-key edges next)
	(incf next))
      (orf id next))

    (assert (not (hash-table-has-key edges id)))
    (let ((from-node (node-info g from))
	  (to-node (node-info g to)))
      (setf (gethash id edges) (make-edge-info data from to))
      (push id (adjacent-edges from-node))
      (push id (adjacent-edges to-node))
      id)))

(defun remove-edge (g id)
  (declare (type graph g) (type id id))
  (dolist (n (incident-nodes g id))
    (declare (type id n))
    (let ((info (node-info g n)))
      (setf (adjacent-edges info) (delete id (adjacent-edges info)))))
  (assert (remhash id (edges g)) nil "Attempted to delete nonexistent edge ~a" id))

(defun remove-node (g id &key remove-edges)
  "Remove node labelled ID.  Node must have no outgoing edges unless remove-edges is true, in which case they're removed first."
  (declare (type graph g) (type id id))
  (let ((edges (adjacent-edge-list g id)))
    (if remove-edges
	(dolist (e edges)
	  (remove-edge g e))
	(assert (null edges) nil "Attempted to delete node ~a with edges ~a where remove-edges was not set." id edges))
    
    ;; This assert should never happen as it should be caught above
    (assert (remhash id (nodes g)))))
      
  

(defun update-node-data (g n k v)
  (declare (type id n) (symbol k))
  (let ((i (node-info g n)))
    (aif (assoc k (node-data i))
	 (setf (cdr it) v)
	 (push (cons k v) (node-data i)))
    v))

(defun update-edge-data (g e k v)
  (declare (type id e) (symbol k) (graph g))
  (let ((i (edge-info g e)))
    (aif (assoc k (edge-data i))
	 (setf (cdr it) v)
	 (push (cons k v) (edge-data i)))
    v))


(defmacro memoize-node-data (g n k form)
  (with-gensyms (i k2 v)
    `(let ((,i (node-info ,g ,n)) (,k2 ,k))
       (aif (assoc ,k2 (node-data ,i))
	    (cdr it)
	    (let ((,v ,form))
	      (push (cons ,k2 ,v) (node-data ,i))
	      ,v)))))

(defmacro memoize-edge-data (g e k form)
  (with-gensyms (i k2 v)
    `(let ((,i (edge-info ,g ,e)) (,k2 ,k))
       (aif (assoc ,k2 (edge-data ,i))
	    (cdr it)
	    (let ((,v ,form))
	      (push (cons ,k2 ,v) (edge-data ,i))
	      ,v)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-graph (&rest args)
  (bind-pprint-args (str g) args
    (pprint-logical-block (str nil :prefix "[" :suffix "]")
      (format str "Graph~2I")
      (dolist (id (node-list g))
	(pprint-pop)
	(let ((info (node-info g id)))
	  (format str "~:@_Node ~a~4I~:[~;~:@_Data: ~:*~a~]~:@_Edges: ~a~2I" 
		  id (node-data info) (adjacent-edges info))))
      (dolist (id (edge-list g))
	(pprint-pop)
	(let ((info (edge-info g id)))
	  (format str "~:@_Edge ~a.  ~a -> ~a.~4I~:[~;~:@_Data: ~:*~a~]~2I"
		  id (from info) (to info) (edge-data info)))))))

(set-pprint-dispatch 'graph #'pprint-graph)


