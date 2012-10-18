
(in-package :cl-tf)

(defclass transform-listener (transformer)
  ((subscriber)))

(defgeneric destroy (obj))

(defmethod initialize-instance :after ((tf transform-listener) &key)
  (with-slots (subscriber tf-prefix) tf
    (setf subscriber (subscribe
                      "/tf" "tf/tfMessage"
                      (lambda (msg)
                        (tf-listener-callback tf msg))))
    (setf tf-prefix (get-param "~tf_prefix" "/"))
    (unless (eql (elt tf-prefix 0) #\/)
      (setf tf-prefix (concatenate 'string "/" tf-prefix)))
    (unless (eql (elt tf-prefix (1- (length tf-prefix))) #\/)
      (setf tf-prefix (concatenate 'string tf-prefix "/")))))

(defun tf-listener-callback (tf msg)
  (mapc (lambda (transform)
          (set-transform tf transform :suppress-callbacks t))
        (tf-message->transforms msg))
  (execute-changed-callbacks tf))

(defmethod destroy ((tf transform-listener))
  (roslisp:unsubscribe (slot-value tf 'subscriber)))
