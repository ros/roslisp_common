(in-package :actionlib-lisp)

;; TODO(Jannik): use API from C++ for the simple action client:
;; https://github.com/ros/actionlib/blob/indigo-devel/include/actionlib/client/simple_action_client.h

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
  "Processes the signal and updates the state-machine and simple-state-machine.
   If the state of the simple state-machine changes the transition callback is
   called."
   (if (and (or (process-signal (stm csm) signal)
                (process-signal (simple-stm csm) signal))
            (transition-cb csm))
       (funcall (transition-cb csm))))

(defmethod comm-state ((csm simple-comm-state-machine))
  "Returns the name of the current state of the simple-comm-state-machine
   as a symbol"
  (name (get-current-state (simple-stm csm))))
  