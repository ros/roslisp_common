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

;;;
;;; Exported
;;;

(defclass simple-action-client (action-client)
  ((goal-handle :initform nil
                :accessor goal-handle))
  (:documentation "Action-client that always only tracks one goal, so no
                   goal-handle is needed and all functions to influence or
                   get information about the goal need the client.
                   The API is based on the API for the simple-action-client
                   in the c-code actionlib."))

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
  "Returns an instance of simple-action-client initialized and subscribes to
   to the topics of the action-server."
  (create-action-client action-name action-type t))


  
(defmethod send-goal ((client simple-action-client) goal-msg &key
                       done-cb active-cb feedback-cb)
  "Sends a goal to the action server.
   `done-cb' Callback that gets called when the goal received a result and is done.
             It takes the state information of the goal and the result as parameters.
   `active-cb' Callback that gets called when the state of the goal changes to active.
               It takes no parameters.
   `feedbak-cb' Callback that gets callback everytime feeback for the goal is received.
                It takes the feedback message as parameter."
  (with-recursive-lock ((client-mutex client))
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
                                                 (funcall feedback-cb feedback))))))
  t)

(defmethod send-goal-and-wait ((client simple-action-client) goal-msg 
                               execute-timeout preempt-timeout)
  "Sends a goal to the action server and loops until the goal is done or
   the `execute-timeout' is reached and then cancels the goal and waits until the
   goal preempted or the `preempt-timeout' is reached. A timeout of 0 implies forever.
   Returns the state information of the goal"
  ;; in case this method is evaporated during execution, unwind-protect
  ;; ensure that we inform the server by canceling the goal
  (let ((clean-finish nil))
    (unwind-protect
         (progn
           (send-goal client goal-msg)
           ;; waiting for execution timeout
           (unless (wait-for-result client execute-timeout)
             (ros-info (actionlib) "Reached execute timeout.")
             (cancel-goal client)
             (with-timeout-handler preempt-timeout
                 #'(lambda ()
                     (ros-info (actionlib) "Reached preempt timeout.")
                     (setf clean-finish t)
                     (return-from send-goal-and-wait (state client)))
               (with-recursive-lock ((csm-mutex (comm-state-machine (goal-handle client))))
                 (loop until (goal-finished client) do
                   (condition-wait (gethash (goal-id (goal-handle client))
                                            (goal-conditions (goal-manager client)))
                                   (csm-mutex (comm-state-machine (goal-handle client))))))))
           ;; returning the state of the action client
           (setf clean-finish t)
           (state client))
      ;; cancel the goal in case we have been evaporated
      (when (and (not clean-finish) (goal-handle client)) ; making sure we still have a goal-handle
        (cancel-goal client)))))
  
(defmethod state ((client simple-action-client))
  "Returns the state information of the goal tracked by the client. 
   RECALLING gets mapped to PENDING and PREEMPTING to ACTIVE.
   Throws an error if the client tracks no goal."
  (goal-handle-not-nil (goal-handle client))
  (let ((status (goal-status (goal-handle client))))
    (cond
      ((eql status :recalling)
       :pending)
      ((eql status :preempting)
       :active)
      (t
       status))))

(defmethod result ((client simple-action-client))
  "Returns the result of the goal tracked by the client. NIL if no result has
   been received yet. Throws an error if the client doesn't track any goal."
  (goal-handle-not-nil (goal-handle client))
  (result (goal-handle client)))

(defmethod cancel-goal ((client simple-action-client))
  "Cancels the goal tracked by the client. Throwns an error if the client
   tracks no goal."
  (goal-handle-not-nil (goal-handle client))
  (cancel (goal-handle client)))

(defmethod stop-tracking-goal ((client simple-action-client))
  "Removes all goals that form the goal-manager and sets the goal-handle to NIL
   so no information about old goals remain."
  (stop-tracking-goals (goal-manager client))
  (setf (goal-handle client) nil) 
  t)

(defmethod wait-for-result ((client simple-action-client) timeout)
  "Waits until a result is received or the timeout is reached. Returns TRUE
   if the goal finished before the timeout or NIL otherwise. An Error is
   thrown if the client hasn't sent a goal yet or stopped tracking it."
  (goal-handle-not-nil (goal-handle client))
  (with-timeout-handler timeout
      #'(lambda () (return-from wait-for-result nil))
    (with-recursive-lock ((csm-mutex (comm-state-machine (goal-handle client))))
      (loop until (is-done client) do
        (condition-wait (gethash (goal-id (goal-handle client))
                                 (goal-conditions (goal-manager client)))
                        (csm-mutex (comm-state-machine (goal-handle client)))))))
  t)


;;;
;;; Internal
;;;

(defun goal-handle-not-nil (goal-handle)
  "Checks if the goal-handle is NIL"
  (assert goal-handle nil
          "The client tracks no goal."))

(defun goal-finished (client)
  "Checks if the goal is in a finished state. This doesn't checks if
a result was received."
  (let ((state (state client)))
    (or (eql state :SUCCEEDED)
        (eql state :LOST)
        (eql state :PREEMPTED))))

(defun is-done (client)
  "Checks if a goal is done."
  (and (eql (comm-state (goal-handle client)) :done)
       (goal-finished client)))
