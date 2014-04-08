(in-package :actionlib)

(defclass state-machine ()
  ((current-state
    :initarg :current-state
    :accessor current-state)
   (states
    :initarg :states
    :initform nil
    :accessor states)
   (stm-mutex :initform (make-mutex :name "state-lock")
              :reader state-mutex)))

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

(defgeneric get-current-state (stm))

(defgeneric set-current-state (stm state))

(defgeneric process-signal (stm signal)
  (:documentation "If the current state has a transition for the signal, sets the 
                   current state to the state following the transition. Returns
                   the new state or nil if there is no transition for the signal."))


(defmethod get-next-state ((stm state-machine) signal)
  (getf (states stm)
        (get-next-state (get-state stm) signal)))

(defmethod get-next-state ((state state) signal)
  (getf (transitions state) signal))

(defmethod get-state ((stm state-machine) &optional state-name)
  (if state-name 
      (getf (states stm) state-name)
      (get-current-state stm)))

(defmethod add-state ((stm state-machine) (state state))
  (push state (states stm)))

(defmethod process-signal ((stm state-machine) signal)
  (let ((next-state (get-next-state stm signal)))
    (if next-state
        (set-current-state stm next-state))))

(defmethod get-current-state ((stm state-machine))
  (with-recursive-lock ((state-mutex stm))
    (current-state stm)))

(defmethod set-current-state ((stm state-machine) (state state))
  (with-recursive-lock ((state-mutex stm))
    (setf (current-state stm) state)))

(defmethod set-current-state ((stm state-machine) state-name)
  (let ((state (get-state stm state-name)))
    (if state
        (with-mutex ((state-mutex stm))
          (setf (current-state stm) state)))))

(defun make-states (state-transitions)
  (let ((result nil))
    (loop for state-transition in state-transitions
          do (push (make-state state-transition)
                   result)
             (push (first state-transition) result))
    result))

(defun make-state (state-transition)
  (make-instance 'state 
                 :name (first state-transition)
                 :transitions (second state-transition)))




                            

  
  

   
