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

(in-package actionlib-lisp)

(defclass simple-action-client (action-client)
  ((goal-handle :accessor goal-handle))
  (:documentation "Like the action-client but simple"))

(defgeneric send-goal-and-wait (client goal-msg execute-timeout preempt-timeout)
  (:documentation "Sends a goal to the action server and waits unitl it's done
                   or a timeout is reached.
                   `client' the client that handles the goal.
                   `goal-msg' goal message send to the server.
                   `execute-timeout' time to wait for the goal to get done until
                                     the client cancels the goal. 0 implies forever.
                   `preempt-timeout' time to wait for the goal to get preemted.
                                     0 implies forever."))

(defgeneric cancel-goal (client)
  (:documentation "Cancels the goal that the client is currently pursuing"))

(defgeneric stop-tracking-goal (client)
  (:documentation "Stops the client from tracking the goal without canceling it."))

(defgeneric state (client)
  (:documentation "Gets the state information for the goal pursued by the client.
                   Possible states are PENDING, ACTIVE, REJECTED, ABORTED, SUCCEEDED
                   and LOST."))

(defgeneric wait-for-result (client timeout)
  (:documentation "Blocks until the goal finishes or the timeout is reached.
                   Returns TRUE if the goal finishes or NIL if the timeout is reached.
                   A timeout of 0 implies wait forever."))


(defun make-simple-action-client (action-name action-type)
  (create-action-client action-name action-type t))

(defun timeout-not-reached (start-time timeout)
  (if (eql timeout 0)
      t
      (> timeout (- (ros-time) start-time))))

(defmethod send-goal ((client simple-action-client) goal-msg &key
                       done-cb active-cb feedback-cb)
  "Sends a goal to the action server"
  (stop-tracking-goal client)
  (setf (goal-handle client)
        (call-next-method client goal-msg
                          :transition-cb #'(lambda (goal-handle)
                                             (if (eql (comm-state goal-handle) :active)
                                                 (if active-cb (funcall active-cb))
                                                 (if (eql (comm-state goal-handle) :done)
                                                     (if done-cb 
                                                         (funcall done-cb (state client)
                                                                  (result goal-handle))))))
                          :feedback-cb #'(lambda (goal-handle feedback)
                                           (declare (ignore goal-handle))
                                           (if feedback-cb
                                               (funcall feedback-cb feedback)))))
  nil)

(defmethod send-goal-and-wait ((client simple-action-client) goal-msg 
                               execute-timeout preempt-timeout)
  (let ((execute-start-time (ros-time))
        (preempt-start-time nil)
        (is-done nil))
    (send-goal client goal-msg 
                :done-cb #'(lambda (state result) 
                             (declare (ignore state result))
                             (setf is-done t)))
    (loop while (and (not is-done)
                     (timeout-not-reached execute-start-time execute-timeout))
          do (sleep 0.01))
    (if (not is-done)
        (cancel-goal client))
    (setf preempt-start-time (ros-time))
    (loop while (and (not is-done)
                     (timeout-not-reached preempt-start-time preempt-timeout))
          do (sleep 0.01))
    (state client)))
  
(defmethod state ((client simple-action-client))
  (let ((status (goal-status (goal-handle client))))
    (cond
      ((eql status :recalling)
       :pending)
      ((eql status :preempting)
       :active)
      (t
       status))))       

(defmethod result ((client simple-action-client))
  (result (goal-handle client)))

(defmethod cancel-goal ((client simple-action-client))
  (cancel (goal-handle client)))

(defmethod stop-tracking-goal ((client simple-action-client))
  (stop-tracking-goals (goal-manager client)))

(defmethod waiting-for-result ((client simple-action-client) timeout)
  (loop while (and (timeout-not-reached (ros-time) timeout)
                   (not (eql (comm-state (goal-handle client)) :done)))
        do (sleep 0.01))
  (eql (comm-state (goal-handle client)) :done))
