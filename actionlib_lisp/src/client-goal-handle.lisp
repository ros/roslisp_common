(in-package :actionlib)

(defvar *terminal-states* '(:rejected :recalled :aborted :succeeded :preempted :lost))

(defclass client-goal-handle ()
  ((comm-state-machine :initarg :comm-state-machine
                       :accessor csm)))

(defgeneric goal-id (goal-handle)
  (:documentation "Returns the id of the goal."))
                  
(defgeneric cancel (goal-handle)
  (:documentation "Sends the Server a message to cancel the goal."))

(defgeneric comm-state (goal-handle)
  (:documentation "Returns the state of the goal's communication
                   state machine."))

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
  (get-goal-id (csm goal-handle)))

(defmethod cancel ((goal-handle client-goal-handle))
  (transition-to (csm goal-handle) :cancel-goal)
  (funcall (send-cancel-fn (csm goal-handle))))
 
(defmethod comm-state ((goal-handle client-goal-handle))
  (comm-state (csm goal-handle)))

(defmethod goal-status ((goal-handle client-goal-handle))
  (latest-goal-status (csm goal-handle)))

(defmethod result ((goal-handle client-goal-handle))
  (latest-result (csm goal-handle)))

(defmethod terminal-state ((goal-handle client-goal-handle))
  (let ((state (name (get-state (csm goal-handle))))
        (status (goal-status goal-handle)))
    (if (and (equal state :done)
             (member status *terminal-states*))
        status)))
      