(in-package :actionlib)

(defclass action-client ()
  ((goal-manager :initarg :goal-manager
                 :accessor goal-manager)
   (goal-pub :initarg :goal-pub
             :accessor goal-pub)
   (cancel-pub :initarg :cancel-pub
               :accessor cancel-pub)
   (action-type :initarg :action-type
                :accessor action-type)
   (seq-nr :initform 0
           :accessor seq)
   (last-status-msg :initform nil
                    :accessor last-status-msg)
   (last-connection :initform nil
                    :accessor last-connection)
   (connection-timeout :initform 2
                       :initarg :connection-timeout
                       :reader connection-timeout))
  (:documentation "TODO"))

(defgeneric send-goal (client goal &optional transition-cb feedback-cb)
  (:documentation "Sends a goal to the action server.
                   `client' is an instance of ACTION-CLIENT.
                   `goal' is an instance of the Goal message.
                   `transitions-cb' Callback that gets called on every
                    state transition for the sent goal. It takes a
                    CLIENT-GOAL-HANDLE as a parameter.
                    `feedback-cb' Callback that gets called evey time the
                    client receives feedback for the sent goal. It takes a
                    CLIENT-GOAL-HANDLE and an instance of the feedback 
                    message as arguments."))

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
  ( :documentation "Returns true if the client is connected to an action 
                    server, NIL otherwise."))


;;;Implementation

(defun feedback-callback (client msg)
  (setf (last-connection client) (ros-time))
  (update-feedbacks (goal-manager client) msg))

(defun status-callback (client msg)
  (setf (last-connection client) (ros-time))
  (setf (last-status-msg client) msg)
  (update-statuses (goal-manager client) msg))

(defun result-callback (client msg)
  (setf (last-connection client) (ros-time))
  (update-results (goal-manager client) msg))

(defun next-seq (client)
  (incf (seq client)))

(defun send-cancel-msg (client goal-id)
  (format t "Cancel ~a~%" goal-id)
  (publish (cancel-pub client)
           (make-message "actionlib_msgs/GoalID"
                         stamp 0
                         id goal-id)))

(defun make-action-client (action-name action-type)
  (let* ((goal-manager (make-instance 'goal-manager))
         (client (make-instance 'action-client
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

(defmethod send-goal ((client action-client) goal-msg &optional 
                                                        transition-cb
                                                        feedback-cb)
  (let ((goal-handle (init-goal (goal-manager client) transition-cb feedback-cb
                                #'(lambda (goal-id) (send-cancel-msg client goal-id)))))
    (publish (goal-pub client)
             (make-message (make-action-type (action-type client) "Goal")
                           (stamp header) (ros-time)
                           (seq header) (next-seq client)
                           (stamp goal_id) (ros-time)
                           (id goal_id) (goal-id goal-handle)
                           goal goal-msg))
    goal-handle))

(defmethod cancel-all-goals ((client action-client))
    (send-cancel-msg client ""))

(defmethod wait-for-server ((client action-client) &optional timeout)
  (let ((start-time (ros-time)))
    (loop while (and (not (is-connected client))
                     (if timeout 
                         (< (- (ros-time) start-time) timeout)
                         t))
          do (sleep 0.02)))
  (is-connected client))

(defmethod is-connected ((client action-client))
  (if (last-connection client)
      (< (- (ros-time) (last-connection client)) 
         (connection-timeout client))))


