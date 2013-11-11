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
  (ensure-both-or-none-supplied target-time-supplied-p fixed-frame-supplied-p
                                target-frame source-frame)
  (handler-case (if (and target-time-supplied-p fixed-frame-supplied-p)
                    (lookup-Transform tf target-frame source-frame source-time timeout
                                      target-time fixed-frame)
                    (lookup-Transform tf target-frame source-frame source-time timeout))
    (tf2-server-error () nil)))

(defmethod lookup-transform ((tf buffer-client) target-frame source-frame 
                             &optional (source-time 0.0) (timeout 0.0) 
                               (target-time source-time target-time-supplied-p)
                               (fixed-frame source-frame fixed-frame-supplied-p))
  (ensure-both-or-none-supplied target-time-supplied-p fixed-frame-supplied-p
                                source-frame target-frame)
  (multiple-value-bind (result status)
      (actionlib:send-goal-and-wait 
       (client tf)
       (actionlib:make-action-goal (client tf)
         :target_frame target-frame :source_frame source-frame
         :source_time source-time :timeout timeout
         :target_time target-time :fixed_frame fixed-frame
         :advanced (and target-time-supplied-p fixed-frame-supplied-p))
       :result-timeout timeout)
    (when (not (eq status :succeeded))
      (error 'tf2-timeout-error :description "Action call did not succeed."))
    (process-result result)))

(defun ensure-both-or-none-supplied (target-time-supplied-p fixed-frame-supplied-p
                                     source-frame target-frame)
  (when (and target-time-supplied-p (not fixed-frame-supplied-p))
    (error 'fixed-frame-missing-error
           :source-frame source-frame
           :target-frame target-frame)))

(defun process-result (result)
  (with-fields (error transform) result
    (with-fields (error error_string) error
      (cond ((eq error 
                 (roslisp-msg-protocol:symbol-code
                  'tf2_msgs-msg:tf2error :lookup_error))
             (error 'tf2-lookup-error () :description error_string))
            ((eq error 
                 (roslisp-msg-protocol:symbol-code
                  'tf2_msgs-msg:tf2error :connectivity_error))
             (error 'tf2-connectivity-error () :description error_string))
            ((eq error 
                 (roslisp-msg-protocol:symbol-code
                  'tf2_msgs-msg:tf2error :extrapolation_error))
             (error 'tf2-extrapolation-error () :description error_string))
            ((eq error 
                 (roslisp-msg-protocol:symbol-code
                  'tf2_msgs-msg:tf2error :invalid_argument_error))
             (error 'tf2-invalid-argument-error () :description error_string))
            ((eq error 
                 (roslisp-msg-protocol:symbol-code
                  'tf2_msgs-msg:tf2error :timeout_error))
             (error 'tf2-timeout-error () :description error_string))
            ((eq error 
                 (roslisp-msg-protocol:symbol-code
                  'tf2_msgs-msg:tf2error :transform_error))
             (error 'tf2-transform-error () :description error_string))
            (t transform)))))