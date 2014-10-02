(in-package :cl-tf)

(defparameter *tf-broadcasting-interval* 0.1)

(defparameter *tf-static-broadcast-future-offset* 0.5)

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
  (let ((msg (make-message
              "tf/tfmessage" 
              :transforms (map 'vector #'transform->msg transforms))))
    (loop-at-most-every interval
      (unless (eq (roslisp:node-status) :running) (return))
      (publish 
       broadcaster 
       (restamp-tf-msg msg (+ (ros-time) *tf-static-broadcast-future-offset*))))))

(defun send-static-transforms (broadcaster interval new-thread &rest transforms)
  (if new-thread
      (sb-thread:make-thread
       #'(lambda ()
           (apply #'send-static-transforms-blocking
                  broadcaster interval transforms))
       :name "TF static broadcaster thread.")
      (apply #'send-static-transforms-blocking broadcaster interval transforms)))

(defmacro with-tf-broadcasting ((broadcaster &rest transforms) &body body)
  (let ((thread-var (gensym)))
    `(let ((,thread-var (send-static-transforms ,broadcaster *tf-broadcasting-interval* t ,@transforms)))
       (unwind-protect ,@body
         (sb-thread:terminate-thread ,thread-var)))))

(defmacro with-tf-broadcasting-list ((broadcaster transforms) &body body)
  (let ((thread-var (gensym)))
    `(let ((,thread-var (apply #'send-static-transforms ,broadcaster *tf-broadcasting-interval* t ,transforms)))
       (unwind-protect ,@body
         (sb-thread:terminate-thread ,thread-var)))))
