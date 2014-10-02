(in-package :actionlib-test)

(defvar *goal-states* '(:rejected :recalled :aborted :succeeded :preempted :lost
                                  :pending :active :recalling :preempting))

(defun terminal-state-test (status)
  (let ((gh (make-instance 'actionlib-lisp::client-goal-handle
                           :comm-state-machine (make-csm nil nil nil))))
    (actionlib-lisp::update-status (actionlib-lisp::comm-state-machine gh) status)
    (if (eql status :lost)
        (assert-true (actionlib-lisp::terminal-state gh))
        (progn
          (assert-false (actionlib-lisp::terminal-state gh))
          (actionlib-lisp::transition-to (actionlib-lisp::comm-state-machine gh) :receive)
          (assert-equal status (actionlib-lisp::terminal-state gh))))))
  
(define-test cancel
  (let* ((cancel-received nil)
         (send-cancel-fn #'(lambda () (setf cancel-received t)))
         (gh (make-instance 'actionlib-lisp::client-goal-handle
                            :comm-state-machine (make-csm nil nil send-cancel-fn))))
    (actionlib-lisp::cancel gh)
    (assert-true cancel-received)
    (assert-equal (actionlib-lisp::comm-state gh) :waiting-for-cancel-ack)))

(define-test terminal-state
  (loop for status in *goal-states*
        do (terminal-state-test status)))