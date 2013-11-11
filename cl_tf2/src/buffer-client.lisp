(in-package :cl-tf2)

(defclass buffer-client ()
  ((client :initarg :client 
           :initform (actionlib:make-action-client
                      "/tf2_buffer_server"
                      "tf2_msgs/LookupTransformAction") 
           :reader client)
   (frequency :initarg :frequency :initform 10 :reader frequency)
   (timeout-padding :initarg :timeout-padding :initform 2 :reader timeout-padding)))