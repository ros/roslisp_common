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

(defclass action-client ()
  ((goal-manager :initarg :goal-manager
                 :accessor goal-manager
                 :documentation "Updates the states of the goals
                                 and executes the callbacks.")
   (goal-pub :initarg :goal-pub
             :accessor goal-pub)
   (cancel-pub :initarg :cancel-pub
               :accessor cancel-pub)
   (action-type :initarg :action-type
                :accessor action-type)
   (seq-nr :initform 0
           :accessor seq-nr)
   (last-status-msg :initform nil
                    :accessor last-status-msg)
   (last-connection :initform nil
                    :accessor last-connection)
   (last-connection-condition :initform (make-waitqueue)
                              :accessor last-connection-condition)
   (connection-timeout :initform 2
                       :initarg :connection-timeout
                       :reader connection-timeout)
   (client-mutex :initform (make-mutex :name (string (gensym "action-client-lock"))) 
                 :reader client-mutex))
  (:documentation "The action-client can send goals to the actio server and monitors
                   their states and execute their callbacks."))

;;;
;;; Exported
;;;

(defgeneric send-goal (client goal-msg &key &allow-other-keys)
  (:documentation "Sends a goal to the action server.
                   `client' is an instance of ACTION-CLIENT.
                   `goal-msg' is an instance of the Goal message."))

(defgeneric cancel-all-goals (client)
  (:documentation "Cancels all goals currently running on the action server."))

(defgeneric cancel-at-and-before-time (client time)
  (:documentation "Cancels all goals currently running on the action
                   server that were stamped at or before `time'."))

(defgeneric wait-for-server (client &optional timeout)
  (:documentation "Waits for the action server to connect to this client
                   or until the timeout is reached. Returns true if the client
                   is connected to the action server or NIL if the timeout is
                   reached."))

(defgeneric is-connected (client)
  (:documentation "Returns true if the client is connected to an action 
                   server, NIL otherwise."))

(defun make-action-client (action-name action-type)
  "Creates and retruns an action-client.
   `acton-name' Name of the action.
   `action-type' Type of the action. Must end with 'Action'."
  (create-action-client action-name action-type nil))

(defmethod send-goal ((client action-client) goal-msg &key 
                                                        transition-cb
                                                        feedback-cb)
  "Sends a goal to the action server and returns the goal-handle.
   `transitions-cb' Callback that gets called on every
                    state transition for the sent goal. It takes a
                    CLIENT-GOAL-HANDLE as a parameter.
   `feedback-cb' Callback that gets called evey time the
                 client receives feedback for the sent goal. It takes a
                 CLIENT-GOAL-HANDLE and an instance of the feedback 
                 message as arguments."
  (let ((goal-handle (init-goal (goal-manager client) transition-cb feedback-cb
                                #'(lambda (goal-id) (send-cancel-msg client goal-id)))))
    (publish (goal-pub client)
             (make-message (make-action-type (action-type client) "Goal")
                           (stamp header) (ros-time)
                           (seq header) (incf (seq-nr client))
                           (stamp goal_id) (ros-time)
                           (id goal_id) (goal-id goal-handle)
                           goal goal-msg))
    goal-handle))

(defmethod cancel-all-goals ((client action-client))
  "Sends a cancel msg with an empty goal-id"
  (send-cancel-msg client ""))

(defmethod wait-for-server ((client action-client) &optional (timeout 0))
  "Loops until the client connects with the action server"
  (with-recursive-lock ((client-mutex client))
    (with-timeout-handler timeout
        #'(lambda () (return-from wait-for-server nil))
      (loop until (is-connected client) do
        (condition-wait (last-connection-condition client)
                        (client-mutex client)))))
  t)

(defmethod is-connected ((client action-client))
  "Checks if the client has recently heard anything from the action server"
  (with-recursive-lock ((client-mutex client))
    (when (last-connection client)
      (< (- (ros-time) (last-connection client)) 
         (connection-timeout client)))))

;;;
;;; Internal
;;;

(defun create-action-client (action-name action-type simple)
  "Creates and retruns an action-client.
   `acton-name' Name of the action.
   `action-type' Type of the action. Must end with 'Action'.
   `simple' If true creates a instance of simple-action-client else action-client."
  (let* ((goal-manager (make-instance 'goal-manager
                                      :csm-type 'comm-state-machine)) 
         (client (make-instance (if simple 
                                    'simple-action-client
                                    'action-client)
                                :goal-manager goal-manager
                                :action-type action-type
                                :goal-pub (advertise (make-action-topic action-name "goal")
                                                     (make-action-type action-type "Goal"))
                                :cancel-pub (advertise (make-action-topic action-name "cancel")
                                                       "actionlib_msgs/GoalID"))))
    (subscribe (make-action-topic action-name "status") "actionlib_msgs/GoalStatusArray"
               #'(lambda (msg) (status-callback client msg)))
    (subscribe (make-action-topic action-name "feedback") (make-action-type action-type "Feedback")
               #'(lambda (msg) (feedback-callback client msg)))
    (subscribe (make-action-topic action-name "result") (make-action-type action-type "Result")
               #'(lambda (msg) (result-callback client msg)))
    client))

(defun feedback-callback (client msg)
  "Callback for the feeback of the action server"
  (update-last-connection client)
  (update-feedbacks (goal-manager client) msg))

(defun status-callback (client msg)
  "Callback for the status messages of the action server"
  (with-recursive-lock ((client-mutex client))
    (update-last-connection client)  
    (setf (last-status-msg client) msg)
    (update-statuses (goal-manager client) msg)
    (notify client)))
    
(defun result-callback (client msg)
  "Callback for the result message of the action server"
  (with-recursive-lock ((client-mutex client))
    (update-last-connection client)
    (update-results (goal-manager client) msg)
    (notify client)))
  

(defun notify (client)
  (when (goal-ids (goal-manager client))
    (dolist (id (goal-ids (goal-manager client)))
      (with-recursive-lock ((csm-mutex (gethash id (goals (goal-manager client)))))
        (when (gethash id (goal-conditions (goal-manager client)))
          (condition-notify (gethash id (goal-conditions (goal-manager client)))))))))


(defun update-last-connection (client)
  "Updates the time of the last communication with the action server"
  (with-recursive-lock ((client-mutex client))
    (setf (last-connection client) (ros-time))
    (condition-notify (last-connection-condition client))))

(defun send-cancel-msg (client goal-id)
  "Publishes a msg with the goal-id on the cancel topic"
  (publish (cancel-pub client)
           (make-message "actionlib_msgs/GoalID"
                         stamp 0
                         id goal-id)))


