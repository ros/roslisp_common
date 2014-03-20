(in-package :actionlib-test)

(defvar *valid-terminal-states* '(:rejected :recalled :aborted :succeeded :preempted))

(defvar *invalid-terminal-states* '(:pending :active :recalling :preempting :lost))

(defun valid-terminal-state-test (status)
  (let ((gh (make-instance 'actionlib::client-goal-handle
                           :comm-state-machine (make-csm))))
    (actionlib::update-status (actionlib::csm gh) status)
    (assert-false (actionlib::terminal-state gh))
    (actionlib::process-signal (actionlib::csm gh) :receive)
    (assert-equal (actionlib::terminal-state gh) status)))

(defun invalid-terminal-state-test (status)
  (let ((gh (make-instance 'actionlib::client-goal-handle
                           :comm-state-machine (make-csm))))
    (actionlib::update-status (actionlib::csm gh) status)
    (assert-false (actionlib::terminal-state gh))
    (actionlib::process-signal (actionlib::csm gh) :receive)
    (assert-false (actionlib::terminal-state gh))))
  
(define-test cancel
  (let ((gh (make-instance 'actionlib::client-goal-handle
                           :comm-state-machine (make-csm))))
    (init-callbacks)
    (assert-false *send-cancel-fn-value*)
    (actionlib::cancel gh)
    (assert-true *send-cancel-fn-value*)
    (assert-equal (actionlib::comm-state gh) :waiting-for-cancel-ack)))

(define-test terminal-state
  (loop for status in *valid-terminal-states*
        do (valid-terminal-state-test status))
  (loop for status in *invalid-terminal-states*
        do (invalid-terminal-state-test status)))

    