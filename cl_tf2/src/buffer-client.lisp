(in-package :cl-tf2)

(defclass buffer-client ()
  ((client :initarg :client 
           :initform (actionlib:make-action-client
                      "/tf2_buffer_server"
                      "tf2_msgs/LookupTransformAction") 
           :reader client)))

(defmethod can-transform ((tf buffer-client) target-frame source-frame 
                          &optional (source-time 0.0) (timeout 0.0)
                            (target-time source-time target-time-supplied-p)
                            (fixed-frame source-frame fixed-frame-supplied-p))
  (declare (ignore tf target-frame source-frame source-time timeout
                   target-time target-time-supplied-p fixed-frame fixed-frame-supplied-p)))

(defmethod lookup-transform ((tf buffer-client) target-frame source-frame 
                             &optional (source-time 0.0) (timeout 0.0) 
                               (target-time source-time target-time-supplied-p)
                               (fixed-frame source-frame fixed-frame-supplied-p))
  (declare (ignore tf target-frame source-frame source-time timeout target-time
                   target-time-supplied-p fixed-frame fixed-frame-supplied-p)))