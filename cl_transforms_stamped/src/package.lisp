
(in-package :cl-user)

#.`(defpackage :cl-transforms-stamped
     (:use :cl :roslisp :cl-transforms)
     (:nicknames :tf-types)
     (:export frame-id stamp child-frame-id source-frame target-frame frame
              stamped transform-stamped pose-stamped point-stamped
              make-pose-stamped make-point-stamped make-transform-stamped
              copy-pose-stamped
              pose->pose-stamped transform->transform-stamped point->point-stamped
              ensure-pose-stamped
              stamped-transform
              make-stamped-transform transform->stamped-transform
              stamped-transform->pose-stamped
              to-msg make-header-msg make-point-msg make-pose-stamped-msg
              from-msg restamp-msg
              pose-stamped->point-stamped-msg
              lookup-transform-stamped transform-pose-stamped transform-point-stamped
              add-new-transform-stamped-callback remove-new-transform-stamped-callback
              with-new-transform-stamped-callback
              transform-stamped-error connectivity-error lookup-error
              extrapolation-error invalid-argument-error timeout-error
              ;; utilities.lisp
              unslash-frame resolve-frame
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms r) (push s r)))))
