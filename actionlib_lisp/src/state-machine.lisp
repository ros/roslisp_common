(in-package :actionlib)

(defclass state-machine ()
  ((current-state
    :initarg :current-state
    :accessor current-state)
   (states
    :initarg :states
    :initform nil
    :accessor states)))

(defclass state ()
  ((name
    :initarg :name
    :reader name)
   (transitions
    :initarg :transitions
    :initform nil
    :accessor transitions)))

(defgeneric get-next-state (state signal))

(defgeneric get-state (stm &optional state-name))

(defgeneric add-state (stm state))

(defmethod get-next-state ((stm state-machine) signal)
  (getf (states stm)
        (get-next-state (get-state stm) signal)))

(defmethod get-next-state ((state state) signal)
  (getf (transitions state) signal))

(defmethod get-state ((stm state-machine) &optional state-name)
  (if state-name 
      (getf (states stm) state-name)
      (current-state stm)))

(defmethod add-state ((stm state-machine) (state state))
  (push state (states stm)))

(defmethod process-signal ((stm state-machine) signal)
  (let ((next-state (get-next-state stm signal)))
    (if next-state
        (setf (current-state stm) next-state))))

(defmethod set-current-state ((stm state-machine) state-name)
  (let ((state (get-state stm state-name)))
    (if state
        (setf (current-state stm) state))))

(defun make-states (state-transitions)
  (let ((result nil))
    (loop for state-transition in state-transitions
          do (push (make-instance 'state 
                                  :name (first state-transition)
                                  :transitions (second state-transition))
                   result)
             (push (first state-transition) result))
    result))




                            

  
  

   
