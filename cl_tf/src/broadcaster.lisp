(in-package :cl-tf)

(defun make-transform-broadcaster (&key (topic "/tf"))
  "Return a publisher that can be used with send-transform"
  (advertise topic "tf/tfMessage"))

(defun send-transform (broadcaster tr)
  "Send a stamped transform."
  (publish broadcaster (transform->tf tr)))

(defun send-transforms (broadcaster &rest transforms)
  "Send stamped transforms."
  (publish broadcaster (transforms->tf transforms)))

(defun send-static-transforms-blocking (broadcaster interval &rest transforms)
  (loop-at-most-every interval
    (unless (eq (roslisp:node-status) :running) (return))
    (publish-msg broadcaster 
                 :transforms (map 'vector 
                                  (lambda (transform) 
                                    (modify-message-copy 
                                     (transform->msg transform) 
                                     (:stamp :header) (ros-time)))
                                  transforms))))

(defun send-static-transforms (broadcaster interval new-thread &rest transforms)
  (if new-thread
      (sb-thread:make-thread
       #'(lambda ()
           (apply #'send-static-transforms-blocking
                  broadcaster interval transforms)))
      (apply #'send-static-transforms-blocking broadcaster interval transforms)))

(defmacro with-tf-broadcasting ((broadcaster &rest transforms) &body body)
  (let ((thread-var (gensym)))
    `(let ((,thread-var (send-static-transforms ,broadcaster 0.01 t ,@transforms)))
       (unwind-protect ,@body
         (sb-thread:terminate-thread ,thread-var)))))