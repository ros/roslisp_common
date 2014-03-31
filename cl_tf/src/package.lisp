
(in-package :cl-user)

#.`(defpackage :cl-tf
       (:use :cl :roslisp :tf-msg :cl-transforms)
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
              stamped-transform pose-stamped point-stamped
              make-pose-stamped make-point-stamped
              make-stamped-transform transform->stamped-transform
              tf-transform->transform tf-message->transforms
              copy-pose-stamped msg->pose msg->pose-stamped
              pose-stamped->msg pose->msg stamped stamped-transform
              pose->pose-stamped pose-stamped point-stamped
              wait-for-transform tf-cache-error tf-connectivity-error
              tf-lookup-error frame-id stamp child-frame-id
              source-frame target-frame frame
              make-transform-broadcaster topic send-transform send-transforms
              send-static-transforms-blocking send-static-transforms with-tf-broadcasting
              msg->point-stamped point-stamped->msg msg->point point->msg
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms r) (push s r)))))
