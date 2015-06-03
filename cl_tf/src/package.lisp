
(in-package :cl-user)

#.`(defpackage :cl-tf
     (:use :cl :roslisp :tf-msg :cl-transforms :cl-transforms-stamped)
     (:nicknames :tf)
     (:shadow transform-pose transform-point)
     (:export transformer make-transformer
              transform-listener
              can-transform lookup-transform set-transform transform-pose transform-point
              execute-changed-callbacks
              tf-msg->transforms transforms->tf-msg transform->tf-msg restamp-tf-msg
              wait-for-transform topic send-transform send-transforms
              make-transform-broadcaster
              send-static-transforms-blocking send-static-transforms with-tf-broadcasting
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms r) (push s r)))
              ,@(let ((r nil))
                  (do-external-symbols (s :cl-transforms-stamped r) (push s r)))
              ))
