(in-package :cl-tf)

(defun make-transform-broadcaster ()
  "Return a publisher that can be used with send-transform"
  (advertise "/tf" "tf/tfMessage")  )

(defun send-transform (broadcaster tr)
  "Send a stamped transform."
  (publish broadcaster (transform->tf tr)))

(defun send-static-transform-blocking (broadcaster tr &key (interval 1.0))
  (loop-at-most-every interval
       (unless (eq (roslisp:node-status) :running)
         (return))
       (let ((m (modify-message-copy (transform->msg tr)
                                     (:stamp :header) (ros-time))))
         (publish-msg broadcaster :transforms (vector m)))))

(defun send-static-transform (broadcaster tr &key (interval 1.0) (new-thread t))
  (if new-thread
      (sb-thread:make-thread
       #'(lambda () (send-static-transform-blocking broadcaster tr :interval interval)))
      (send-static-transform-blocking broadcaster tr :interval interval)
      ))