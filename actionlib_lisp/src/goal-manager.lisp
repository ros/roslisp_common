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

(defclass goal-manager ()
  ((goals :initform (make-hash-table :test #'equal)
          :accessor goals
          :documentation "Hashtable with all comm-state-machines of all
                          monitored goals.")
   (goal-ids :initform nil
             :accessor goal-ids
             :documentation "List of all goal-ids of all monitored goals.")
   (waiting-for-goal-ack-timeout :initform 10
                                 :initarg waiting-for-goal-ack-timeout
                                 :accessor waiting-for-goal-ack-timeout
                                 :documentation "Time in seconds to wait for server
                                                 to acknowledge the goal before
                                                 goal-status is set to lost.")
   (csm-type :initform 'comm-state-machine
             :initarg :csm-type
             :reader csm-type)
   (id-counter :initform -1
               :accessor id-counter
               :documentation "Counter that gets included into the goal-id.")
   (hash-mutex :initform (make-mutex :name (string (gensym "goal-hash-table-lock")))
               :reader hash-mutex)
   (id-mutex :initform (make-mutex :name (string (gensym "goal-ids-lock")))
             :reader id-mutex))
  (:documentation "Contains all comm-state-machines and updates them if new messages 
                   arrive from the action server."))

(defgeneric init-goal (manager transition-cb feedback-cb cancel-fn)
  (:documentation "Initializes a new goal and returns the goal-handle."))

(defgeneric update-statuses (manager status-array)
  (:documentation "Reads the information from the status-array and updates the
                   comm-state-machines accordingly."))

(defgeneric update-results (manager action-result)
  (:documentation "Updates the with the action-result associated comm-state-machine
                   with the result message."))

(defgeneric update-feedbacks (manager action-feedback)
  (:documentation "Updates the with the action-feedback associated comm-state-machine
                   with the feedback message."))

(defgeneric stop-tracking-goals (manager)
  (:documentation "Removes all goals from the goal manager"))

(defgeneric goal-with-id (manager id)
  (:documentation "Returns the entry in the goal-hash-tablefor the goal with the given id."))

;;;Implementation
(defun status-msg->id-status (status-msg)
  "Gets a status msg and returns a the id and status."
  (with-fields (status (id (id goal_id))) status-msg
    (let ((status-symbol (car (rassoc status
                                      (symbol-codes 'actionlib_msgs-msg:GoalStatus)))))
      (values id status-symbol))))

(defun generate-goal-id (manager)
  "Generates a new unique goal-id."
  (incf (id-counter manager))
  (format nil "~a-~a-~a" *ros-node-name* (id-counter manager) (ros-time)))

(defmethod init-goal ((manager goal-manager) transition-cb feedback-cb cancel-fn)
  "Creates a new comm-state-machine and goal-handle and returns the goal-handle"
  (let* ((goal-id (generate-goal-id manager))
         (goal-handle (make-instance 'client-goal-handle))
         (csm (make-instance (csm-type manager)
                             :goal-id goal-id
                             :transition-cb (if transition-cb 
                                                #'(lambda () (funcall transition-cb goal-handle)))
                             :feedback-cb (if feedback-cb
                                              #'(lambda (feedback) 
                                                  (funcall feedback-cb goal-handle feedback)))
                             :send-cancel-fn #'(lambda () (funcall cancel-fn goal-id)))))
    (setf (comm-state-machine goal-handle) csm)
    (with-recursive-lock ((hash-mutex manager))
      (setf (gethash goal-id (goals manager)) csm))
    (with-recursive-lock ((id-mutex manager))
      (push goal-id (goal-ids manager)))
    goal-handle))

(defmethod update-statuses ((manager goal-manager) status-array)
  "Updates the statuses of all goals that the goal-manager tracks. If the status 
   array contains the goal-id of comm-state-machine, the state of the comm-state-machine
   gets updated with the status else the comm-state-machine gets set to lost."
  (let ((goal-ids (with-recursive-lock ((id-mutex manager))
                    (copy-list (goal-ids manager)))))
    (with-fields ((status-list status_list)) status-array
      ;;loops over the goals in the status list, updates their status
      ;;and removes them from the local goal-ids list
      (loop for goal-status being the elements of status-list
            do (multiple-value-bind (id status-symbol) (status-msg->id-status goal-status)
                 (multiple-value-bind (comm-state-machine has-state-machine-p) (goal-with-id manager id)
                   (when has-state-machine-p
                     (setf goal-ids (remove id goal-ids :test #'equal))
                     (update-status comm-state-machine status-symbol)))))
      ;;loops over the remaining ids in goal-ids and marks them as lost if they aren't
      ;;waiting-for-goal-ack or if they exceeded the timeout
      (loop for goal-id in goal-ids
            do (let ((csm (nth-value 0 (goal-with-id manager goal-id))))
                 (when (and csm
                            (or (not (eql (name (get-current-state (stm csm)))
                                          :waiting-for-goal-ack))
                                (> (- (ros-time) (start-time csm)) 
                                   (waiting-for-goal-ack-timeout manager))))
                   (with-recursive-lock ((id-mutex manager))
                     (setf (goal-ids manager) 
                           (remove goal-id (goal-ids manager) :test #'equal)))
                   (update-status csm :lost)))))))

(defmethod update-results ((manager goal-manager) action-result)
  "Updates the comm-state-machine with the goal-id from the result message."
  (with-fields (status result) action-result
    (multiple-value-bind (id status-symbol) (status-msg->id-status status)
      (multiple-value-bind (comm-state-machine has-state-machine-p) (goal-with-id manager id)
        (when has-state-machine-p
          (update-status comm-state-machine status-symbol)
          (update-result comm-state-machine result))))))
    
(defmethod update-feedbacks ((manager goal-manager) action-feedback)
  "Updates the comm-state-machine with the goal-id from the feedback message."
  (with-fields (status feedback) action-feedback
    (multiple-value-bind (id status-symbol) (status-msg->id-status status)
      (multiple-value-bind (comm-state-machine has-state-machine-p) (goal-with-id manager id)
        (when has-state-machine-p
          (update-status comm-state-machine status-symbol)
          (update-feedback comm-state-machine feedback))))))

(defmethod stop-tracking-goals ((manager goal-manager))
  "Removes all comm-state-machines and goal-ids."
  (setf (goal-ids manager) nil)
  (setf (goals manager) (make-hash-table :test #'equal)))

(defmethod goal-with-id ((manager goal-manager) id)
  "Returns the values for `id' in the goals-hash-table."
  (with-recursive-lock ((hash-mutex manager))
    (gethash id (goals manager))))
