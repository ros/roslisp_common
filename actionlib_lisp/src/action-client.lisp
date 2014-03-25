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
   (connected :initform nil
              :reader is-connected
              :accessor connected
              :documentation "Returns true if the client is 
               connected to an action server, NIL otherwise."))
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

(defgeneric wait-for-action-server-to-start (client &optional timeout)
  (:documentation "Waits for the action server to connect to this client
                   or until the timeout is reached"))


;;;Implementation

(defun feedback-callback (client msg)
  (update-feedbacks (goal-manager client) msg))

(defun status-callback (client msg)
  (setf (last-status-msg client) msg)
  (if (not (connected client))
      (setf (connected client) t))
  (update-statuses (goal-manager client) msg))

(defun result-callback (client msg)
  (update-results (goal-manager client) msg))

(defun get-cancel-fn (goal-id)
  (format t "Cancel ~a~%" goal-id))

(defun next-seq (client)
  (incf (seq client)))

(defun make-action-client (action-name action-type)
  (let* ((goal-manager (make-instance 'goal-manager
                                      :cancel-fn #'get-cancel-fn))
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
  (let ((goal-handle (init-goal (goal-manager client) transition-cb feedback-cb)))
    (publish (goal-pub client)
             (make-message (make-action-type (action-type client) "Goal")
                           (stamp header) (ros-time)
                           (seq header) (next-seq client)
                           (stamp goal_id) (ros-time)
                           (id goal_id) (goal-id goal-handle)
                           goal goal-msg))
    goal-handle))

(defmethod cancel-all-goals ((client action-client))
    (publish (cancel-pub client)
             ;; Cancel all policy encoded by stamp=0 and id=""
             (make-message "actionlib_msgs/GoalID"
                           stamp 0
                           id "")))


