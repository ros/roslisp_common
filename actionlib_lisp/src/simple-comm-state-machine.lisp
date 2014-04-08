(in-package :actionlib)

(defparameter *simple-states*
  (make-states '((:done (:send-goal :pending))
                 (:pending (:active :active
                            :preempting :active
                            :lost :done
                            :receive :done))
                 (:active (:recalling :pending
                           :lost :done
                           :receive :done)))))

(defclass simple-comm-state-machine (comm-state-machine)
  ((simple-stm :initform (make-instance 'state-machine 
                                        :current-state (getf *simple-states* :pending)
                                        :states *simple-states*)
               :accessor simple-stm))
  (:documentation "Like the comm-state-machine but it includes another state machine
                   that summarizes the other states into pending, active and done."))

(defmethod transition-to ((csm simple-comm-state-machine) signal)
  (when (and (process-signal (stm csm) signal)
             (process-signal (simple-stm csm) signal))
    (if (transition-cb csm)
        (funcall (transition-cb csm)))))

(defmethod comm-state ((csm simple-comm-state-machine))
  (name (get-current-state (simple-stm csm))))
  