(in-package :actionlib-test)

(defvar *valid-terminal-states* '(:rejected :recalled :aborted :succeeded :preempted :lost))

(defvar *invalid-terminal-states* '(:pending :active :recalling :preempting))

(defun valid-terminal-state-test (status)
  (let ((gh (make-instance 'actionlib::client-goal-handle
                           :comm-state-machine (make-csm nil nil nil))))
    (actionlib::update-status (actionlib::csm gh) status)
    (if (eql status :lost)
        (assert-true (actionlib::terminal-state gh))
        (progn
          (assert-false (actionlib::terminal-state gh))
          (actionlib::transition-to (actionlib::csm gh) :receive)
          (assert-equal status (actionlib::terminal-state gh))))))

(defun invalid-terminal-state-test (status)
  (let ((gh (make-instance 'actionlib::client-goal-handle
                           :comm-state-machine (make-csm nil nil nil))))
    (actionlib::update-status (actionlib::csm gh) status)
    (assert-false (actionlib::terminal-state gh))
    (actionlib::transition-to (actionlib::csm gh) :receive)
    (assert-false (actionlib::terminal-state gh))))
  
(define-test cancel
  (let* ((cancel-received nil)
         (send-cancel-fn #'(lambda () (setf cancel-received t)))
         (gh (make-instance 'actionlib::client-goal-handle
                            :comm-state-machine (make-csm nil nil send-cancel-fn))))
    (actionlib::cancel gh)
    (assert-true cancel-received)
    (assert-equal (actionlib::comm-state gh) :waiting-for-cancel-ack)))

(define-test expect-terminal-state
  (loop for status in *valid-terminal-states*
        do (valid-terminal-state-test status)))

(define-test expect-no-terminal-state
  (loop for status in *invalid-terminal-states*
        do (invalid-terminal-state-test status)))