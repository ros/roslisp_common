(in-package :actionlib)

(defclass action-client ()
  ((goal-manager :accessor goal-manager
                 :initform nil)
   (action-type :initarg :action-type
                :accessor action-type)
   (goal-pub :initarg :goal-pub
             :accessor goal-pub)
   (cancel-pub :initarg :cancel-pub
               :accessor cancel-pub)
   (last-status-msg :accessor last-status-msg
                    :initform nil))

(defgeneric send-goal (client goal &optional transition-cb feedback-cb)
  "Sends a goal to the action server.
   `client' is an instance of ACTION-CLIENT.
   `goal' is an instance of the Goal message.
   `transitions-cb' Callback that gets called on every state transition
   for the sent goal. It takes a CLIENT-GOAL-HANDLE as a parameter.
   `feedback-cb' Callback that gets called evey time the client receives
   feedback for the sent goal. It takes a CLIENT-GOAL-HANDLE and an 
   instance of the feedback message as arguments.")

(defgeneric cancel-all-goals (client)
  "Cancels all goals currently running on the action server.")

(defgeneric cancel-at-and-before-time (client time)
  "Cancels all goals currently running on the action server that
   were stamped at or before `time'.")

(defgeneric wait-for-action-server-to-start (client &optional timeout)
  "Waits for the action server to connect to this client or until the 
   timeout is reached")

(defgeneric is-connected (client)
  "Returns true if the cleint is connected to an action server,
   NIL otherwise.")


;;;Implementation

(defun feedback-callback (client msg)
  (format t "FEEDBACK: ~a~%" msg))

(defun status-callback (client msg))
  (with-fields ((status-list status_list)) msg
    (loop for goal-status being the elements of status-list
          do (with-fields (status (id (id goal_id))) goal-status
               (format t "ID: ~a STATUS ~a~%" id status)))))

(defun result-callback (client msg)
  (format t "RESULT: ~a~%" msg))

(defmacro make-action-goal (client &body args)
  `(make-message (action-goal-type (action-type ,client))
                 ,@args))

(defun make-action-client (action-name action-type)
  (let ((client (make-instance 'action-client
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
                                                        done-cb feedback-cb 
                                                        active-cb state-change-cb)
  (setf (goal-handle client)
        (make-instance 'client-goal-handle 
                       :done-cb done-cb
                       :feedback-cb feedback-cb
                       :active-cb active-cb
                       :state-change-cb state-change-cb))                       
  (publish (goal-pub client)
           (make-message (make-action-topic (action-type client) "Goal")
                         (stamp header) (ros-time)
                         (seq header) (incf (seq-nr client))
                         (stamp goal_id) (ros-time)
                         (id goal_id) 
                         goal *goal*)))