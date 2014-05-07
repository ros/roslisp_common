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

(defclass client-goal-handle ()
  ((comm-state-machine :initarg :comm-state-machine
                       :accessor comm-state-machine))
  (:documentation "Handle for the goal that mainly wraps around the comm-state-machine."))
                 
(defgeneric cancel (goal-handle)
  (:documentation "Sends the Server a message to cancel the goal."))

(defgeneric goal-status (goal-handle)
  (:documentation "Returns the status of the goal as defined in 
                   actionlib_msgs/GoalStatus."))

(defgeneric result (goal-handle)
  (:documentation "Returns the result produced by the action server
                   for the goal or NIL if no result was received
                   for the goal."))

(defgeneric terminal-state (goal-handle)
  (:documentation "Returns the terminal state information of the
                   goal as an integer from the GoalStatus message.
                   NIL if the goal isn't done."))

;;;
;;; Exported
;;;

(defmethod goal-id ((goal-handle client-goal-handle))
  "Returns the id of the goal"
  (goal-id (comm-state-machine goal-handle)))

(defmethod cancel ((goal-handle client-goal-handle))
  "Makes a calls the :cancel-goal transition to change the state
   of the goal and sends a cancel message to the server."
  (transition-to (comm-state-machine goal-handle) :cancel-goal)
  (funcall (send-cancel-fn (comm-state-machine goal-handle))))
 
(defmethod comm-state ((goal-handle client-goal-handle))
  "Returns the state of the comm-state-machine"
  (comm-state (comm-state-machine goal-handle)))

(defmethod goal-status ((goal-handle client-goal-handle))
  "Returns the status of the goal as it stands in the status-msg from
   the server."
  (latest-goal-status (comm-state-machine goal-handle)))

(defmethod result ((goal-handle client-goal-handle))
  "Returns the last result the client received for the goal."
  (latest-result (comm-state-machine goal-handle)))

(defmethod terminal-state ((goal-handle client-goal-handle))
  "Retuns the state of the goal if the goal is done else nil."
  (let ((state (comm-state (comm-state-machine goal-handle)))
        (status (goal-status goal-handle)))
    (if (equal state :done)
        status
        nil)))
      