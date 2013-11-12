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
            (t (from-msg transform))))))