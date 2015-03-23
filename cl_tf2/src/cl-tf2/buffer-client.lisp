;;; Copyright (c) 2013, 2015 Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(defparameter *tf2-buffer-server-goal* "/tf2_buffer_server")
(defparameter *tf2-buffer-server-goal-type* "tf2_msgs/LookupTransformAction")
(defparameter *tf2-wait-for-server-timeout* 2.0)

(defclass buffer-client ()
  ((client :initarg :client :reader client
           :type actionlib-lisp:simple-action-client
           :initform (actionlib-lisp:make-simple-action-client
                      *tf2-buffer-server-goal*
                      *tf2-buffer-server-goal-type*))
   (lock :accessor lock :type mutex
         :initform (sb-thread:make-mutex
                    :name (string (gensym "TF2-BUFFER-CLIENT-LOCK-"))))))

(defmethod initialize-instance :after ((buffer buffer-client) &key (verbosity :warn))
  "Ensures that we have waited for the action server `buffer' connected to."
  (unless (actionlib-lisp:wait-for-server (client buffer) *tf2-wait-for-server-timeout*)
    (case verbosity
      (:warn (roslisp:ros-warn "tf2/buffer-client" "Wait for server timed out."))
      (:error (roslisp:ros-error "tf2/buffer-client" "Wait for server timed out.")))))

(defun make-buffer-client (action-goal)
  "Creates a tf buffer-client connecting to buffer-server at `action-goal'."
  (let ((client (actionlib-lisp:make-simple-action-client
                 action-goal *tf2-buffer-server-goal-type*)))
  (make-instance 'buffer-client :client client)))

;;;
;;; SIMPLE API
;;;

(defmethod lookup-transform ((buffer buffer-client) target-frame source-frame
                             time timeout)
 "Uses buffer-client 'buffer' to query tf for a transform from 'source-frame'
 to 'target-frame' at 'time'.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available."
  (with-slots (client lock) buffer
    (sb-thread:with-recursive-lock (lock)
      (let ((goal-msg (actionlib-lisp:make-action-goal-msg client
                        :target_frame (unslash-frame target-frame)
                        :source_frame (unslash-frame source-frame)
                        :source_time time :timeout timeout)))
        (actionlib-lisp:send-goal-and-wait client goal-msg 0.1 0.0) ; bug: does not work with timeout 0.0
        (actionlib-lisp:wait-for-result client 0.0)
        (process-result client)))))

;;;
;;; INTERNAL
;;;

;; TOOD(Georg): consider moving this function into roslisp-msg-protocol
(defun code-symbols (msg-type code)
  "Retrieves the list of symbol-code associations which contain 'code' for 'msg-type'."
  (remove code (roslisp-msg-protocol:symbol-codes msg-type) :test-not #'= :key #'rest))

;; TOOD(Georg): consider moving this function into roslisp-msg-protocol
(defun code-symbol (msg-type code)
  "Retrieves the first symbol associated with 'code' in the symbol codes of 'msg-type'."
  (let ((symbol-codes (code-symbols msg-type code)))
    (when symbol-codes
      (caar symbol-codes))))

(defun unslash-frame (frame)
  "Removes any leading or trailing '/' characters from the string
`frame' and returns the resulting string."
  (string-trim "/" frame))

(defun process-result (client)
 "Process the result returned to 'client' from the buffer-server. Either raises an
 appropriate error or returns the transform stamped."
  (unless (eql (actionlib-lisp:state client) :succeeded)
    (error 'tf2-buffer-client-error :description "Action call did not succeed."))
  (with-fields (error transform) (actionlib-lisp:result client)
    (with-fields (error error_string) error
      (case (code-symbol 'tf2_msgs-msg:tf2error error)
        (:lookup_error (error 'tf2-lookup-error :description error_string))
        (:connectivity_error (error 'tf2-connectivity-error :description error_string))
        (:extrapolation_error (error 'tf2-extrapolation-error :description error_string))
        (:invalid_argument_error (error 'tf2-invalid-argument-error :description error_string))
        (:timeout_error (error 'tf2-timeout-error :description error_string))
        (:transform_error (error 'tf2-transform-error :description error_string))
        (t transform)))))
