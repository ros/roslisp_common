
(in-package :cl-tf)

(defun transforms->tf-msg (transforms)
  (make-message "tf/tfMessage" :transforms (map 'vector #'to-msg transforms)))

(defun transform->tf-msg (tr)
  (make-message "tf/tfMessage" :transforms (vector (to-msg tr))))

(defun tf-msg->transforms (tf-msgs)
  "Return the transform that corresponds to a tf message."
  (loop for msg across (tf-msg:transforms tf-msgs) collecting (from-msg msg)))

(defun restamp-tf-msg (msg new-stamp)
  (with-slots (tf-msg:transforms) msg
    (loop for transform-msg across tf-msg:transforms do
      (restamp-msg transform-msg new-stamp))
    msg))
