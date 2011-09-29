(in-package :cl-prob)

(defun event-function (event)
  (typecase event
    (function event)
    (list #'(lambda (x) (member x event :test #'equal)))
    (otherwise (partial #'equal event))))

(defclass event () ())

(defclass interval (event)
  ((left :initarg :left :reader left :type extended-real)
   (right :initarg :right :reader right :type extended-real)
   (left-open :initarg :left-open :initform nil :reader left-open)
   (right-open :initarg :right-open :initform nil :reader right-open)))


