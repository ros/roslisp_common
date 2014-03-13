(in-package :actionlib)

(defvar *state* :done)
(defvar *done-transitions* (list :send-goal :waiting-for-goal-ack))
(defvar *w-f-g-ack-transitions* (list :cancel-goal :waiting-for-cancel-ack
                                      :pending :pending
                                      :active :active))
(defvar *pending-transitions* (list :active :active
                                    :cancel-goal :waiting-for-cancel-ack
                                    :recalling :recalling
                                    :rejected :rejected))
(defvar *active-transitions* (list :cancel-goal :waiting-for-cancel-ack
                                   :preempting :preempting
                                   :aborted :waiting-for-result
                                   :succeeded :waiting-for-result))
(defvar *state-transitions* (list :done *done-transitions*
                                  :waiting-for-goal-ack *w-f-g-ack-transitions*
                                  :pending *pending-transitions*
                                  :active *active-transitions*))

(defun transit (transition)
  (setf *state* (getf (get-transitions *state*) transition)))

(defun get-transitions (state)
  (getf *state-transitions* state))

(defun set-state (state)
  (setf *state* state))