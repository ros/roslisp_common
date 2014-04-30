;;; Copyright (c) 2014, Jannik Buckelo <jannikbu@cs.uni-bremen.de>
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

(in-package :actionlib-lisp)

(defparameter *states* 
  ;;              State   Signal     Target-State
  (make-states '((:done (:send-goal :waiting-for-goal-ack))
                 (:waiting-for-goal-ack (:cancel-goal :waiting-for-cancel-ack
                                         :pending :pending
                                         :active :active
                                         :recalling :recalling
                                         :preempting :preempting
                                         :rejected :waiting-for-result
                                         :recalled :waiting-for-result
                                         :preempted :waiting-for-result
                                         :succeeded :waiting-for-result
                                         :aborted :waiting-for-result
                                         :receive :done
                                         :lost :done))
                 (:pending (:cancel-goal :waiting-for-cancel-ack
                            :active :active
                            :recalling :recalling
                            :preempting :preempting
                            :rejected :waiting-for-result
                            :recalled :waiting-for-result
                            :preempted :waiting-for-result
                            :succeeded :waiting-for-result
                            :aborted :waiting-for-result
                            :receive :done
                            :lost :done))
                 (:active (:cancel-goal :waiting-for-cancel-ack
                           :preempting :preempting
                           :preempted :waiting-for-result
                           :succeeded :waiting-for-result
                           :aborted :waiting-for-result
                           :receive :done
                           :lost :done))
                 (:waiting-for-cancel-ack (:recalling :recalling
                                           :preempting :preempting
                                           :rejected :waiting-for-result
                                           :recalled :waiting-for-result
                                           :preempted :waiting-for-result
                                           :succeeded :waiting-for-result
                                           :aborted :waiting-for-result
                                           :receive :done
                                           :lost :done))
                 (:recalling (:preempting :preempting
                              :rejected :waiting-for-result
                              :recalled :waiting-for-result
                              :preempted :waiting-for-result
                              :succeeded :waiting-for-result
                              :aborted :waiting-for-result
                              :receive :done
                              :lost :done))
                 (:preempting (:preempted :waiting-for-result
                               :succeeded :waiting-for-result
                               :aborted :waiting-for-result
                               :receive :done
                               :lost :done))
                 (:waiting-for-result (:receive :done
                                       :lost :done)))))

(defclass comm-state-machine ()
  ((stm :initform (make-instance 'state-machine 
                                 :current-state (getf *states* :waiting-for-goal-ack)
                                 :states *states*)
        :accessor stm
        :documentation "Manages the state and statetransitions.")
   (goal-id :initarg :goal-id
            :reader goal-id)
   (start-time :initform (ros-time)
               :accessor start-time)
   (transition-cb :initarg :transition-cb
                  :initform nil
                  :accessor transition-cb)
   (feedback-cb :initarg :feedback-cb
                :initform nil
                :accessor feedback-cb)
   (send-cancel-fn :initarg :send-cancel-fn
                   :reader send-cancel-fn)
   (latest-goal-status :initform :pending
                       :accessor latest-goal-status)
   (latest-result :initform nil
                  :accessor latest-result)
   (latest-feedback :initform nil
                    :accessor latest-feedback)
   (stm-mutex :initform (make-mutex :name (string (gensym "state-machine-lock")))
              :reader stm-mutex)
   (status-mutex :initform (make-mutex :name (string (gensym "status-lock")))
                 :reader status-mutex) 
   (result-mutex :initform (make-mutex :name (string (gensym "result-lock")))
                 :reader result-mutex) 
   (feedback-mutex :initform (make-mutex :name (string (gensym "feedback-lock")))
                   :reader feedback-mutex))
  (:documentation "Monitors the state of the communication between action-client
                   and the server for one goal and executes the callbacks."))

(defgeneric transition-to (csm signal)
  (:documentation "Processes the signal and executes the transition-callback if
                   necessary"))

(defgeneric update-status (csm status)
  (:documentation "Updates the state with the given status."))

(defgeneric update-result (csm action-result)
  (:documentation "Updates the state with the given result."))

(defgeneric update-feedback (csm action-feedback)
  (:documentation "Updates the state with the given feedback and executes the
                   feedback callback."))

(defgeneric comm-state (goal-handle)
  (:documentation "Returns the state of the goal's communication
                   state machine."))

;;; Implementation

(defmethod transition-to ((csm comm-state-machine) signal)
  "Tranists to the next state given the signal and calls the
   transition-callback. If the result was processed before the 
   last status update the transition-callback gets called even
   if the state-machine doesn't change"
  (if (and (or (eql (name (get-current-state (stm csm))) :done)
               (process-signal (stm csm) signal))
           (transition-cb csm))
      (funcall (transition-cb csm))))

(defmethod update-status ((csm comm-state-machine) status)
  "If the status is not equal to the last status the comm-state-machine
   gets updated with the new status"
  (when (not (eql (latest-goal-status csm) status))
    (with-recursive-lock ((status-mutex csm))
      (setf (latest-goal-status csm) status)))
  (if (get-next-state (stm csm) status)
        (transition-to csm status)))
      
(defmethod update-result ((csm comm-state-machine) action-result)
  "Updates the result of the comm-state-machine"
  (with-recursive-lock ((result-mutex csm))
    (setf (latest-result csm) action-result))
  (transition-to csm :receive))

(defmethod update-feedback ((csm comm-state-machine) action-feedback)
  "Updates the latest feedback of the comm-state-machine and calls 
   the feedback-callback"
  (with-recursive-lock ((feedback-mutex csm))
    (setf (latest-feedback csm) action-feedback))
  (if (feedback-cb csm)
      (funcall (feedback-cb csm) action-feedback)))

(defmethod comm-state ((csm comm-state-machine))
  "Returns the name of the current state of the comm-state-machine
   as a symbol"
  (name (get-current-state (stm csm))))
  
