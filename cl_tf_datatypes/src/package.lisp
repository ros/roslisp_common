
(in-package :cl-user)

#.`(defpackage :cl-tf-datatypes
     (:use :cl :roslisp :cl-transforms)
     (:nicknames :tf-types)
     (:export stamped-transform pose-stamped point-stamped
              make-pose-stamped make-point-stamped
              make-stamped-transform copy-pose-stamped
              stamped stamped-transform
              pose-stamped point-stamped
              frame-id stamp child-frame-id
              source-frame target-frame frame
              transform->stamped-transform
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms r) (push s r)))))
