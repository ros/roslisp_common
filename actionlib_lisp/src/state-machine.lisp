(in-package :actionlib)

(defclass state-machine ()
  ((current-state
    :initarg :current-state
    :initform :done)
   (states
    :initarg :states
    :initform nil)))

(defclass state ()
  ((name
    :initarg :name)
   (transitions
    :initarg :transitions
    :initform nil)))

(defgeneric get-next-state (state signal)

(defgeneric get-state (stm &optional state-name))

(defgeneric add-state (stm state))

(defgeneric add-states (stm states))

(defmethod get-next-state ((state state-machine) signal)
  (get-next-state (slot-value state 'current-state) signal))

(defmethod get-next-state ((state state) signal)
  (getf (slot-value state 'transitions) signal))

(defmethod get-state ((stm state-machine) &optional state-name)
  (if state-name 
      (getf (slot-value stm 'states) state-name)
      (slot-value stm 'current-state)))

(defmethod add-state ((stm state-machine) (state state))
  (push state (slot-value stm 'states)))

(defun make-states (state-transitions)
  (let ((result nil))
    (loop for state-transition in state-transitions
          do (push (make-instance 'state 
                                  :name (first state-transition)
                                  :transitions (second state-transition))
                   result)
             (push (first state-transition) result))
    result))
                            

  
  

   
