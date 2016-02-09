;;; Copyright (c) 2013, Georg Bartels <georg.bartels@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to
;;; endorse or promote products derived from this software without specific
;;; prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-tf2)

(defparameter *buffer-server-action-topic* "tf2_buffer_server")
(defparameter *buffer-server-action-type* "tf2_msgs/LookupTransformAction")

(defclass buffer-client ()
  ((client :initarg :client
           :initform (make-simple-action-client
                      *buffer-server-action-topic*
                      *buffer-server-action-type*)
           :reader client)
   (lock :initform (sb-thread:make-mutex :name (string (gensym "TF2-LOCK-")))
         :accessor lock :type mutex)))

(defmethod initialize-instance :after ((client buffer-client) &key (timeout 0.0))
  (unless (wait-for-server (client client) timeout)
    (error 'timeout-error :description "Waiting for action server timed out.")))
  
(defmethod lookup-transform ((tf buffer-client) target-frame source-frame
                             &key time timeout target-time fixed-frame)
  (declare (type string target-frame source-frame)
           (type (or number null) time timeout target-time)
           (type (or string null) fixed-frame))
  ;; extra NIL checks are here in case the passed argument is explicitly set to NIL
  (let ((source-time (or time 0.0))
        (timeout (or timeout 0.0))
        (target-time (or target-time 0.0))
        (target-frame (unslash-frame target-frame))
        (source-frame (unslash-frame source-frame))
        (fixed-frame (unslash-frame fixed-frame)))
    (sb-thread:with-recursive-lock ((lock tf))
      (let* ((action-client (client tf))
             (goal-msg (make-action-goal-msg action-client
                         :target_frame target-frame
                         :source_frame source-frame
                         :source_time source-time
                         :timeout timeout
                         :target_time target-time
                         :fixed_frame fixed-frame
                         :advanced fixed-frame)) ; <- FIXED-FRAME is converted to a boolean here
             (status
               (send-goal-and-wait action-client goal-msg timeout timeout))
             (result (result action-client)))
        (unless (eq status :succeeded)
          (error 'transform-stamped-error :description
                 (if result
                     (roslisp:msg-slot-value (roslisp:msg-slot-value result :error) :error_string)
                     (format nil "Action call did not succeed. Status: ~a" status))))
        (process-result result)))))

(defun process-result (result)
  (with-fields (error transform) result
    (with-fields (error error_string) error
      (ecase (roslisp-msg-protocol:code-symbol 'tf2_msgs-msg:tf2error error)
        (:no_error (from-msg transform))
        (:lookup_error (error 'lookup-error :description error_string))
        (:connectivity_error (error 'connectivity-error :description error_string))
        (:extrapolation_error (error 'extrapolation-error :description error_string))
        (:invalid_argument_error (error 'invalid-argument-error :description error_string))
        (:timeout_error (error 'timeout-error :description error_string))
        (:transform_error (error 'transform-stamped-error :description error_string))))))

(defmethod transform-pose-stamped ((tf buffer-client)
                                   &key target-frame pose timeout use-current-ros-time)
  (check-type target-frame string)
  (check-type pose pose-stamped)
  (let ((target-frame (unslash-frame target-frame))
        (time (if use-current-ros-time
                  (roslisp:ros-time)
                  (or (stamp pose) 0.0)))
        (source-frame (frame-id pose)))
    (let ((transform (lookup-transform tf target-frame source-frame
                                       :time time :timeout timeout)))
      (pose->pose-stamped
       target-frame
       (stamp transform)
       (cl-transforms:transform-pose transform pose)))))

(defmethod transform-point-stamped ((tf buffer-client)
                                    &key target-frame point timeout use-current-ros-time)
  (check-type target-frame string)
  (check-type point point-stamped)
  (let ((target-frame (unslash-frame target-frame))
        (time (if use-current-ros-time
                  (roslisp:ros-time)
                  (or (stamp point) 0.0)))
        (source-frame (frame-id point)))
    (let ((transform (lookup-transform tf target-frame source-frame
                                       :time time :timeout timeout)))
      (point->point-stamped
       target-frame
       (stamp transform)
       (cl-transforms:transform-point transform point)))))
