;; TODO(Jannik): add license header

(in-package :actionlib)

(defvar *terminal-states* '(:rejected :recalled :aborted :succeeded :preempted :lost))

(defclass client-goal-handle ()
  ((comm-state-machine :initarg :comm-state-machine
                       :accessor comm-state-machine)))

(defgeneric goal-id (goal-handle)
  (:documentation "Returns the id of the goal."))
                  
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


;;;Implementation

(defmethod goal-id ((goal-handle client-goal-handle))
  (get-goal-id (comm-state-machine goal-handle)))

(defmethod cancel ((goal-handle client-goal-handle))
  (transition-to (comm-state-machine goal-handle) :cancel-goal)
  (funcall (send-cancel-fn (comm-state-machine goal-handle))))
 
(defmethod comm-state ((goal-handle client-goal-handle))
  (comm-state (comm-state-machine goal-handle)))

(defmethod goal-status ((goal-handle client-goal-handle))
  (latest-goal-status (comm-state-machine goal-handle)))

(defmethod result ((goal-handle client-goal-handle))
  (latest-result (comm-state-machine goal-handle)))

(defmethod terminal-state ((goal-handle client-goal-handle))
  (let ((state (comm-state (comm-state-machine goal-handle)))
        (status (goal-status goal-handle)))
    (if (and (equal state :done)
             (member status *terminal-states*))
        status)))
      