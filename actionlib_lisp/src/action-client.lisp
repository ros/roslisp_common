(in-package :actionlib)

(defclass action-client ()
  ((state-machine :initform (make-instance 'action-client-stm)
                  :accessor state-machine)
   (goal-handle :accessor goal-handle)
   (goal-pub :initarg :goal-pub
             :accessor goal-pub)
   (cancel-pub :initarg :cancel-pub
               :accessor cancel-pub)
   (action-type :initarg :action-type
                :reader action-type)
   (seq-nr :initform 0
           :accessor seq-nr)))

(defclass client-goal-handle ()
  ((goal-id :reader goal-id 
            :initarg :goal-id)
   (timestamp :reader timestamp 
              :initarg :timestamp
              :initform (ros-time))
   (result :reader result 
           :initform nil
           :documentation "The result of the goal or NIL.")
   (done-cb :reader done-callback 
            :initarg :done-cb
            :documentation "The done callback of the goal. It must
                            have type FUNCTION and takes exactly two parameter,
                            the terminal state and the result of the action.")
   (feedback-cb :reader feedback-callback 
                :initarg :feedback-cb
                :initform nil
                :documentation "The feedback callback of the goal. It
                                is of type FUNCTION and takes exactly one 
                                parameter, the feedback of the action.")
   (state-change-cb :reader state-change-callback 
                    :initarg :state-change-cb
                    :initform nil
                    :documentation "Callback that gets called whenever
                                   the state of the goal transitions on the status
                                   topic.")))

(defgeneric send-goal (client goal-msg &optional
                                         done-cb feedback-cb active-cb
                                         state-change-cb)
  (:documentation "Sends a goal to the action server.

`client' is an instance of ACTION-CLIENT.

`done-cb' is a function that takes exactly two parameters, the
final status and the result of the goal
`feedback-cb' is a function that takes exactly one parameter, the
feedback message of the goal.

`active-cb' is a function with no parameters.

`state-change-cb' is a function with one parameter that is called
whenever the state of the goal transitions. The parameter is
indicating the new status."))

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