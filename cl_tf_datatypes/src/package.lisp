
(in-package :cl-user)

#.`(defpackage :cl-tf-datatypes
     (:use :cl :roslisp :cl-transforms)
     (:nicknames :tf-types)
     (:export frame-id stamp child-frame-id source-frame target-frame frame
              stamped transform-stamped pose-stamped point-stamped
              make-pose-stamped make-point-stamped make-transform-stamped
              copy-pose-stamped
              pose->pose-stamped transform->transform-stamped point->point-stamped
              stamped-transform
              make-stamped-transform transform->stamped-transform
              stamped-transform->pose-stamped
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms r) (push s r)))))
