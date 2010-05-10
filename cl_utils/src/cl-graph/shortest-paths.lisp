(in-package :cl-graph)

(defun compute-navfn (g dest &key (cost-key :length))
  "Compute navigation function in G for destination DEST, represented as a hashtable from NODE to extended reals.  Secondary return value maps from NODE to outgoing edge and can be used to follow path.  Assumes positive costs."
  (let ((navfn (make-hash-table))
	(routing-table (make-hash-table))
	(q (make-priority-queue))
	(closed (make-hash-table)))

    (labels ((set-cost-to-go (n c)
	       (setf (gethash n navfn) c)
	       (enqueue q n (- c)))
	     
	     (process-edge (n e)
	       (let* ((n2 (other-node g e n))
		      (node-cost (gethash n navfn))
		      (edge-cost (lookup-edge-data g cost-key e))
		      (new (e+ node-cost edge-cost)))
		 (when (e<= new (gethash n2 navfn 'infinity))
		   (set-cost-to-go n2 new)
		   (setf (gethash n2 routing-table) e)))))

      (set-cost-to-go dest 0.0)
      (setf (gethash dest routing-table) 'goal)
      (while (> (num-entries q) 0)
	(let ((n (dequeue-highest q)))
	  (unless (hash-table-has-key closed n)
	    (dolist (e (adjacent-edge-list g n) (values navfn routing-table))
	      (process-edge n e))
	    (setf (gethash n closed) t))))
	  
      (values navfn routing-table))))


	      
	      
(defun extract-path (g table src)
  "Extract path from routing table (represented as list of edges, and list of nodes).  Error if there exists no path."
  (let ((nodes (list src)) (edges nil))
    (loop
       (let* ((n (car nodes))
	      (next (gethash n table)))
	 (assert next nil "~a not found in table" n)
	 (when (eql 'goal next)
	   (return (values (nreverse edges) (nreverse nodes))))
	 (push next edges)
	 (push (other-node g next n) nodes)))))
	     
	 
(defun shortest-path (g dest src &key (cost-key :length))
  "Return shortest path from src to dest, represented as 1) a list of edge ids 2) a list of node ids"
  (extract-path g (nth-value 1 (compute-navfn g dest :cost-key cost-key)) src))

(defun shortest-path-distance (g src dest &key (cost-key :length))
  "Return the shortest path distance between src and dest, or infinity if there's no path"
  (gethash src (compute-navfn g dest :cost-key cost-key) 'infinity))



(defun diameter (g)
  ;; Probably better to use all-pairs-shortest-paths once
  (reduce #'emax (node-list g) :key (partial #'radius-from g)))
	  
(defun radius-from (g n)
  (let ((nav-fn (compute-navfn g n)))
    (reduce #'emax (node-list g)
	    :key #'(lambda (n2)
		     (if (eql n2 n) 
			 0
			 (gethash n2 nav-fn 'infinity))))))
    