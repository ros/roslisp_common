;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Software License Agreement (BSD License)
;; 
;; Copyright (c) 2008, Willow Garage, Inc.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with 
;; or without modification, are permitted provided that the 
;; following conditions are met:
;;
;;  * Redistributions of source code must retain the above 
;;    copyright notice, this list of conditions and the 
;;    following disclaimer.
;;  * Redistributions in binary form must reproduce the 
;;    above copyright notice, this list of conditions and 
;;    the following disclaimer in the documentation and/or 
;;    other materials provided with the distribution.
;;  * Neither the name of Willow Garage, Inc. nor the names 
;;    of its contributors may be used to endorse or promote 
;;    products derived from this software without specific 
;;    prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH 
;; DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :actionlib)

;;; We simplify the action state machine
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *action-client-transitions*
    '((:waiting-for-ack -> :pending :active :done)
      (:pending -> :pending :active :done)
      (:active  -> :active :done)
      (:done    -> :done))
    "Valid transitions of the action client.")

  (defparameter *server-state-mappings*
    '((:pending :recalling -> :pending)
      (:active :preempting -> :active)
      (:preempted :rejected :recalled :aborted :succeeded :lost
       -> :done))
    "Mapping from goal states to simple client states."))

(defvar *action-server-timeout* 2.0
  "Time in seconds to wait for ack. If no goal ack is received within
  timeout, the goal is set to :lost.")

(define-condition server-lost (simple-error) ()
  (:documentation "Indicates that the server seems to be dead."))

(define-condition feedback-signal (condition)
  ((goal :reader goal :initarg :goal)
   (feedback :reader feedback :initarg :feedback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass client-goal-handle ()
  ((simple-state :accessor simple-state :initform :waiting-for-ack)
   (action-state :accessor action-state :initform nil)
   (goal-id :reader goal-id :initarg :goal-id)
   (timestamp :reader timestamp :initarg :timestamp)
   (result :reader result :initform nil
           :documentation "The result of the goal or NIL.")
   (client :reader client :initarg :client)
   (done-cb :reader done-callback :initarg :done-cb
            :documentation "The done callback of the goal. It must
            have type FUNCTION and takes exactly two parameter, the
            terminal state and the result of the action.")
   (feedback-cb :reader feedback-callback :initarg :feedback-cb
                :initform nil
                :documentation "The feedback callback of the goal. It
                is of type FUNCTION and takes exactly one parameter,
                the feedback of the action.")
   (active-cb :reader active-callback :initarg :active-cb
              :initform nil
              :documentation "The active callback that gets called
              when the goal gets active. It is a function object
              without any parameters.")
   (mutex :initform (make-mutex :name (string (gensym "ACTION-GOAL-"))))
   (condition :initform (make-waitqueue :name (string (gensym "ACTION-GOAL-"))))))

(defclass action-client ()
  ((goals :initform (make-hash-table :test 'equal)
          :documentation "The hash table of all goals that are
                          currently tracked by this action client,
                          indexed by the goal id.")
   (mutex :initform (make-mutex :name (string (gensym "ACTION-CLIENT-"))))
   (condition :initform (make-waitqueue :name (string (gensym "ACTION-CLIENT-"))))
   (action-type :reader client-action-type :initarg :action-type)
   (seq-nr :accessor seq-nr :initform 0)
   (goal-pub :accessor goal-pub :initarg :goal-pub)
   (cancel-pub :accessor cancel-pub :initarg :cancel-pub)
   (active-time :accessor active-time :initform nil)))

(defgeneric action-client-transition (new-simple-state goal)
  (:documentation "Method that is called on every transition of a
  goal. 

  `new-simple-state' is either :PENDING, :ACTIVE or :DONE.
  `goal' is the goal, already containing the new goal state."))

(defgeneric send-goal (client goal-msg done-cb &optional feedback-cb active-cb)
  (:documentation "Sends a goal to the action server. 

   `client' is an instance of ACTION-CLIENT. 

   `done-cb' is a function that takes exactly two parameters, the
   final status and the result of the goal
   
   `feedback-cb' is a function that takes exactly one parameter, the
   feedback message of the goal.

   `active-cb' is a function with no parameters."))

(defgeneric cancel-goal (goal)
  (:documentation "Cancels a goal."))

(defgeneric wait-for-server (client &optional timeout)
  (:documentation "Wait for the action server to come up. Waits at
  most `timeout' seconds."))

(defgeneric wait-for-result (goal &optional timeout)
  (:documentation "Blocks until the goal's result is ready and returns
  it. Returns NIL after `timeout'"))

(defgeneric call-goal (client goal &key timeout feedback-cb)
  (:documentation "Calls the goal and blocks until a result is
  available or the timeout occurs. If the timeout occurs, returns the
  values NIL and :TIMEOUT. Otherwise, returns the result and the
  terminal action-state. If `feedback-cb' is specified, calls it in
  the caller's thread for every feedback. Also raises the signal
  FEEDBACK-SIGNAL for every feedback."))

(defgeneric connected-to-server (client)
  (:method ((client action-client))
    (with-recursive-lock ((slot-value client 'mutex))
      (and (slot-value client 'active-time)
           (< (- (ros-time) (slot-value client 'active-time))
              *action-server-timeout*))))
  (:documentation "Returns T if the client is connected to an action
  server, otherwise NIL."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookkeeping functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-goal (client goal)
  (let ((id (goal-id goal)))
    (with-slots (mutex goals) client
      (with-recursive-lock (mutex)
        (assert (not (gethash id goals)) ()
                "Goal is already registered.")
        (setf (gethash id goals) goal)))))

(defun has-goal (client goal)
  (with-slots (mutex goals) client
    (with-recursive-lock (mutex)
      (not (null (gethash (goal-id goal) goals))))))

(defun get-goal (client goal &key (fail-without-goal t))
  (with-slots (mutex goals) client
    (with-recursive-lock (mutex)
      (when fail-without-goal
        (assert (has-goal client goal)))
      (gethash (goal-id goal) goals))))

(defun remove-goal (client goal)
  (if (has-goal client goal)
      (with-slots (mutex goals) client
        (with-recursive-lock (mutex)
          (remhash (goal-id goal) goals)))
      (ros-warn (remove-goal lisp-action-client)
                "Trying to remove goal `~a' which is unknown."
                (goal-id goal))))

(defun make-goal-id ()
  (remove #\. (format nil "~a_GOAL_~10,5$" *ros-node-name* (ros-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-new-states (transitions current-state)
  "Returns the list of possible states that can be reached from
current state, based on `transitions'."
  (cond ((eq (caar transitions) '->)
         (find-new-states (cdr transitions) current-state))
        ((eq (caar transitions) current-state)
         (cdr (member '-> (car transitions))))
        (transitions
         (find-new-states (cons (cdar transitions) (cdr transitions))
                          current-state))))

(defun perform-state-transition (goal new-msg-state)
  "Checks the transition for validity and performs the
  transition. Executes the method ACTION-CLIENT-TRANSITION if a
  transition of simple client state has been made."
  (check-type goal client-goal-handle)
  (let* ((old-client-state (simple-state goal))
         (old-action-state (action-state goal))
         (possible-new-client-states (find-new-states
                                      *action-client-transitions*
                                      old-client-state))
         (new-client-state (car (find-new-states *server-state-mappings*
                                                 new-msg-state))))
    (when (not (member new-client-state possible-new-client-states))
      (ros-warn (action-client state)
                "Invalid transition from [~a] to [~a]. Terminating goal."
                old-client-state new-client-state)
      (setf new-client-state :done
            new-msg-state :lost))
    (set-client-goal-states goal new-client-state new-msg-state)
    (unless (and (eq old-client-state new-client-state)
                 (eq old-action-state new-msg-state))
      (action-client-transition new-client-state goal)))
  ;; Something interesting might have happened. Pulse the wait condition.
  (with-recursive-lock ((slot-value goal 'mutex))
    (condition-broadcast (slot-value goal 'condition))))

(defun set-client-goal-states (goal simple-state action-state)
  "Updates the slots `simple-state' and `action-state' of `goal'."
  (with-recursive-lock ((slot-value goal 'mutex))
    (setf (simple-state goal) simple-state
          (action-state goal) action-state))
  goal)

(defun maybe-finish-goal (goal)
  "Checks if the goal is completely finished and calls the result
  callback in that case. Completely finished means that the goal must
  be in :done state and a result must be available or the goal must be
  in state :lost."
  (let (fun result state)
    (with-recursive-lock ((slot-value goal 'mutex))
      (when (and (or (eq (action-state goal) :lost)
                     (result goal))
                 (eq (simple-state goal)
                     :done))
        (setf fun (done-callback goal)
              result (result goal)
              state (action-state goal))))
    (when fun
      (remove-goal (client goal) goal)
      (funcall fun state result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun client-status-callback (client status-array-msg)
  (flet ((find-lost-goals (goal-status-msgs)
           (mapcar #'cdr
                   (remove-if (rcurry #'member (mapcar #'goal-id goal-status-msgs)
                                      :test #'equal)
                              (hash-table-to-alist (slot-value client 'goals))
                              :key #'car)))
         (update-goal-status (goal-status-msg)
           "Takes a goal status msg, pre-processes it and calls
            PERFORM-STATE-TRANSITION."
           (perform-state-transition (get-goal client goal-status-msg)
                                     (get-status-symbol goal-status-msg))))
    (let ((status-list (map 'list #'identity (status_list-val status-array-msg))))
      (with-recursive-lock ((slot-value client 'mutex))
        (let ((lost-goals (find-lost-goals status-list)))
          (loop for lost-goal in lost-goals do
            (when (> (- (ros-time) (timestamp lost-goal))
                     *action-server-timeout*)
              (ros-warn (action-client state)
                        "Goal `~a' is lost. Terminating goal." (goal-id lost-goal))
              (perform-state-transition (get-goal client lost-goal) :lost))))
        (loop for status-msg in status-list do
          (when (has-goal client status-msg)
            (update-goal-status status-msg))))))
  (with-mutex ((slot-value client 'mutex))
    (setf (active-time client) (ros-time))
    (condition-broadcast (slot-value client 'condition))))

(defun client-result-callback (client result)
  (with-fields ((result-msg result)
                (status-msg status))
      result
    (when (has-goal client status-msg)
      (let ((goal (get-goal client status-msg)))
        (setf (slot-value goal 'result) result-msg)
        (maybe-finish-goal goal)
        (with-recursive-lock ((slot-value goal 'mutex))
          (condition-broadcast (slot-value goal 'condition))))))
  (with-mutex ((slot-value client 'mutex))
    (setf (active-time client) (ros-time))    
    (condition-broadcast (slot-value client 'condition))))

(defun client-feedback-callback (client feedback)
  (with-mutex ((slot-value client 'mutex))
    (with-fields ((feedback-msg feedback)
                  (status-msg status))
        feedback
      (when (has-goal client status-msg)
        (let ((goal (get-goal client status-msg :fail-without-goal nil)))
          (when (feedback-callback goal)
            (funcall (feedback-callback goal) feedback-msg)))))
    (setf (active-time client) (ros-time))    
    (condition-broadcast (slot-value client 'condition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action client implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action-client-transition ((state (eql :pending)) goal)
  nil)

(defmethod action-client-transition ((state (eql :active)) goal)
  (let ((fun (active-callback goal)))
    (when fun (funcall fun))))

(defmethod action-client-transition ((state (eql :done)) goal)
  (maybe-finish-goal goal))

(defun make-action-client (action-name action-type)
  (check-type action-name string)
  (check-type action-type string)
  (let ((client (make-instance 'action-client
                               :action-type action-type
                               :goal-pub (advertise (action-topic action-name "goal")
                                                    (action-type action-type "Goal"))
                               :cancel-pub (advertise (action-topic action-name "cancel")
                                                      "actionlib_msgs/GoalID"))))
    (subscribe (action-topic action-name "status") "actionlib_msgs/GoalStatusArray"
               (partial #'client-status-callback client))
    (subscribe (action-topic action-name "feedback") (action-type action-type "Feedback")
               (partial #'client-feedback-callback client))
    (subscribe (action-topic action-name "result") (action-type action-type "Result")
               (partial #'client-result-callback client))
    client))

(defmethod send-goal ((client action-client) goal-msg done-cb &optional feedback-cb active-cb)
  (assert (connected-to-server client) ()
          "Not connected to an action server. Cannot send goal.")
  (with-slots (mutex goals goal-pub action-type seq-nr) client
    (let* ((now (ros-time))
           (goal (make-instance 'client-goal-handle
                                :done-cb done-cb
                                :feedback-cb feedback-cb                                 
                                :active-cb active-cb
                                :client client
                                :goal-id (make-goal-id)
                                :timestamp now)))
      (register-goal client goal)
      (publish goal-pub (make-message (action-type action-type "Goal")
                                      (stamp header) now
                                      (seq header) (incf seq-nr)
                                      (stamp goal_id) now
                                      (id goal_id) (goal-id goal)
                                      goal goal-msg))
      goal)))

(defmethod cancel-goal ((goal client-goal-handle))
  (with-slots (cancel-pub) (client goal)
    (publish cancel-pub (make-instance 'actionlib_msgs-msg:<goalid>
                                       :id (goal-id goal)))))

(defmethod wait-for-server ((client action-client) &optional timeout)
  (flet ((wait ()
           (with-recursive-lock ((slot-value client 'mutex))
             (loop until (connected-to-server client) do
               (condition-wait (slot-value client 'condition)
                               (slot-value client 'mutex))))))
    (handler-case
        (if timeout
            (sb-ext:with-timeout timeout
              (wait))
            (wait))
      (sb-ext:timeout (c)
        (declare (ignore c))
        (return-from wait-for-server nil)))
    ;; Give the sockets time to settle down.
    (sleep 2)
    t))

(defmethod wait-for-result ((goal client-goal-handle) &optional timeout)
  (flet ((wait ()
           (with-recursive-lock ((slot-value goal 'mutex))
             (loop until (and goal (result goal)
                              (eq (simple-state goal) :done))
                   unless (connected-to-server (client goal)) do
                     (error 'server-lost
                            :format-control "Client lost connection to action server.")
                   do (condition-wait (slot-value goal 'condition)
                                      (slot-value goal 'mutex))
                   finally (return (result goal))))))

    (let ((start-time (ros-time)))
      (loop do
        (handler-case
            (sb-ext:with-timeout *action-server-timeout*
              (if timeout
                  (handler-case
                      (sb-ext:with-timeout (- (+ start-time timeout)
                                              (ros-time))
                        (wait))
                    (sb-ext:timeout (c)
                      (declare (ignore c))
                      (return-from wait-for-result nil)))
                  (return-from wait-for-result (wait))))
          (sb-ext:timeout (e)
            (declare (ignore e))))))))

(defmethod call-goal ((client action-client) goal &key timeout feedback-cb)
  (let ((mutex (make-mutex))
        (condition (make-waitqueue))
        (current-feedback nil)
        (result-data nil)
        (goal-handle nil)
        (start-time (ros-time)))
    (flet ((feedback-callback (feedback)
             (with-mutex (mutex)
               (setf current-feedback feedback)
               (condition-broadcast condition)))
           (result-callback (status result)
             (with-mutex (mutex)
               (setf result-data (list result status))
               (condition-broadcast condition))))
      (unwind-protect
           (progn
             (setf goal-handle (send-goal client goal #'result-callback #'feedback-callback))
             (loop do
               (handler-case
                   (sb-ext:with-timeout *action-server-timeout*
                     (with-mutex (mutex)
                       (cond ((not (connected-to-server client))
                              (error 'server-lost
                                     :format-control "Client lost connection to server."))
                             (result-data
                              (return-from call-goal (apply #'values result-data)))
                             (current-feedback
                              (when feedback-cb
                                (funcall feedback-cb current-feedback))
                              (restart-case 
                                  (signal 'feedback-signal
                                          :goal goal-handle
                                          :feedback current-feedback)
                                (abort-goal (&optional result)
                                  :report "Preempt the goal."
                                  (return-from call-goal (values result :aborted))))
                              (setf current-feedback nil))
                             ((and timeout (<= (- (+ start-time timeout) (ros-time))
                                               0))
                              (return-from call-goal (values nil :timeout))))
                       (if timeout
                           (sb-ext:with-timeout (- (+ start-time timeout)
                                                   (ros-time))
                             (condition-wait condition mutex))
                           (condition-wait condition mutex))))
                 (sb-ext:timeout (e)
                   (declare (ignore e))))))
        (unless (or (not goal-handle) (eq (simple-state goal-handle) :done))
          (cancel-goal goal-handle))))))
