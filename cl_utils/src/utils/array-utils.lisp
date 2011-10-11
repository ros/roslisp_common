(in-package :cl-utils)

(defun make-adjustable-vector ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defmacro do-vec ((x v &optional r) &body body)
  "Like dolist except for a vector"
  (let ((l (gensym)) (vec (gensym)))
    `(let ((,vec ,v))
       (dotimes (,l (length ,vec) ,r)
         (let ((,x (aref ,vec ,l)))
           ,@body)
         ))))