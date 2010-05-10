(defpackage :priority-queue
  (:documentation "Package priority-queue (pqueue)

A simple implementation of priority queues using heaps (represented as arrays).  Insertion and removal is O(log n) and peeking at the top element is O(1).


Operations
----------
make-priority-queue
peek-highest
dequeue-highest
enqueue
num-entries")
  (:export
   :priority-queue
   :make-priority-queue
   :peek-highest :dequeue-highest :enqueue :num-entries)
  (:nicknames :pqueue)
  (:use :cl :cl-utils :extended-reals))

(in-package :pqueue)

(defstruct (pq-entry (:conc-name nil) (:constructor make-pq-entry (item priority)) (:type list))
  item 
  priority)


(deftype priority-queue ()
  'vector)

(defun make-priority-queue ()
  "make-priority-queue
Return an empty priority-queue"
  (make-array 0 :adjustable t :fill-pointer 0))

(defun num-entries (pq)
  (length pq))

(defun peek-highest (pq)
  "peek-highest PQ.  Return the highest priority element of PQ without removing it, and its priority.  Signals error if PQ is empty."
  (assert (> (length pq) 0) nil "Attempted to call peek-highest on empty priority queue.")
  (values (item (aref pq 0)) (priority (aref pq 0))))
		

(defun enqueue (pq item priority)
  "enqueue PQ ITEM PRIORITY.  Add a new entry to the PQ.  PRIORITY may be an extended real."
  (let ((n (vector-push-extend (make-pq-entry item priority) pq)))
    (till (zerop n)
      (let ((m (parent n)))
	(unless (e> priority (priority (aref pq m)))
	  (return-from enqueue nil))
	(rotatef (aref pq m) (aref pq n))
	(setf n m)))))

(defun dequeue-highest (pq)
  "dequeue-highest PQ.  Remove and return the highest priority element of PQ.  Signals error if PQ is empty."
  (let ((n (length pq)))
    (assert (> n 0) nil "Attempted to call dequeue-highest on empty priority queue.")
  
    ;; Save the first element of the current array to return at end
    (let ((item-to-return (aref pq 0)))
    
      ;; Remove first element and maintain heap property
      (labels ((helper (i)
		 "Percolate down from node I as necessary."
		 (let ((best-child nil)
		       (best-val (priority (aref pq i))))
		   
		   ;; Figure out the best child
		   (dotimes (j 2)
		     (let ((k (+ 1 j (* 2 i))))
		       (when (< k (1- n))
			 (let ((val (priority (aref pq k))))
			   (when (e> val best-val)
			     (setf best-child k
				   best-val val))))))

		   ;; If necessary, percolate down and recurse
		   (when best-child
		     (rotatef (aref pq i) (aref pq best-child))
		     (helper best-child)))))
	
	;; Insert new element at root and percolate down from there
	(setf (aref pq 0) (vector-pop pq))
	(unless (= n 1)
	  (helper 0))
	(values (item item-to-return) (priority item-to-return))))))


(defun parent (i)
  (assert (> i 0) nil "Cannot find parent of 0")
  (floor (1- i) 2))
