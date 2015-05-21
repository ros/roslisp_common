
(in-package :cl-user)

#.`(defpackage :cl-tf
     (:use :cl :roslisp :tf-msg :cl-transforms :cl-transforms-stamped)
     (:nicknames :tf)
     (:shadow transform-pose transform-point)
     (:export transformer make-transformer
              transform-listener
              can-transform lookup-transform
              set-transform transform-pose
              add-transforms-changed-callback
              remove-transforms-changed-callback
              with-transforms-changed-callback
              execute-changed-callbacks
              transform-point
              transform->stamped-transform
              tf-transform->transform tf-message->transforms
              msg->pose msg->pose-stamped
              pose-stamped->msg pose->msg
              wait-for-transform topic send-transform send-transforms
              send-static-transforms-blocking send-static-transforms with-tf-broadcasting
              msg->point-stamped point-stamped->msg msg->point point->msg
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms r) (push s r)))
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms-stamped r) (push s r)))
              ))
