(in-package actionlib-lisp)

(defclass simple-action-client (action-client)
  ((goal-handle :accessor goal-handle))
  (:documentation "Like the action-client but simple"))

(defgeneric cancel-goal (client))

(defun make-simple-action-client (action-name action-type)
  (create-action-client action-name action-type t))

(defmethod send-goal2 ((client simple-action-client) goal-msg
                       done-cb active-cb feedback-cb)
  (cancel goal-handle)
  (send-goal client 
             #'(lambda (goal-handle)
                 (if (comm-state :active)
                     (funcall active-cb)
                     (if (comm-state :done)
                         (funcall done-cb (goal-status goal-handle)
                                  (result goal-handle)))))
             #'(lambda (goal-handle feedback)
                 (funcall feedback-cb feedback))))

(defmethod result ((client simple-action-client))
  (result (goal-handle client)))

(defmethod cancel-goal ((client simple-action-client))
  (cancel (goal-handle client)))

