(in-package :action-lib)

(defclass client-goal-handle ()
  ((comm-state-machine :initarg comm-state-machine
                       :reader comm-state-machine)))

(defgeneric goal-id (goal-handle)
  "Returns the id of the goal.")

(defgeneric cancel (goal-handle)
  "Sends the Server a message to cancel the goal.")

(defgeneric comm-state (goal-handle)
  "Returns the state of the goal's communication state machine.")

(defgeneric goal-status (goal-handle)
  "Returns the status of the goal as defined in actionlib_msgs/GoalStatus.")

(defgeneric result (goal-handle)
  "Returns the result produced by the action server for the goal or
   NIL if no result was received for the goal.")

(defgeneric terminal-state (goal-handle)
  "Returns the terminal state information of the goal as an integer
   from the GoalStatus message. NIL if the goal isn't done.")


;;;Implementation

(defmethod goal-id ((goal-handle client-goal-handle))
  nil)

(defmethod cancel ((goal-handle client-goal-handle))
  nil)

(defmethod comm-state ((goal-handle client-goal-handle))
  nil)

(defmethod goal-status ((goal-handle client-goal-handle))
  nil)

(defmethod result ((goal-handle client-goal-handle))
  nil)

(defmethod terminal-state ((goal-handle client-goal-handle))
  nil)