(in-package :actionlib-test)

(defparameter *valid-transitions* '(:send-goal :cancel-goal :pending :active 
                                    :recalling :preempting :rejected :aborted
                                    :succeeded :recalled :preempted :receive))

(defparameter *transitions-to-waiting-for-result* 
  (list '(:rejected :waiting-for-result)
        '(:recalled :waiting-for-result)
        '(:preempted :waiting-for-result)
        '(:aborted :waiting-for-result)
        '(:succeeded :waiting-for-result)
        '(:receive :done)
        '(:lost :done)))

(defun make-transitions-to (state transition-names)
  (let ((transitions nil))
    (loop for name in transition-names
          do (push (list name state) transitions))))

(defun test-state-transitions (state transitions)
  (let ((test-transitions nil))
    (loop for transition in transitions
          do (setf (getf test-transitions (first transition))
                   (second transition)))
    (loop for transition-name in *valid-transitions*
          do (test-state-transition state
                                    (list transition-name
                                          (getf test-transitions 
                                                transition-name))))))

(defun test-state-transition (state transition)
  (let ((stm (actionlib-lisp::stm (make-instance 'actionlib-lisp::comm-state-machine)))
        (target-state nil)
        (target-state-name nil))
    (actionlib-lisp::set-current-state stm state)
    (setf target-state (actionlib-lisp::process-signal stm (first transition)))
    (if target-state
        (setf target-state-name (actionlib-lisp::name target-state)))
    (assert-equal (second transition) target-state-name)))


;;; Test for the state machine

;;TODO(Jannik) this test fails until you recompile it, no clue why it would do that
(define-test waiting-for-goal-ack-transitions
  (test-state-transitions :waiting-for-goal-ack
                          (append (list '(:cancel-goal :waiting-for-cancel-ack)
                                        '(:active :active)
                                        '(:pending :pending)
                                        '(:recalling :recalling)
                                        '(:preempting :preempting)) 
                                  *transitions-to-waiting-for-result*)))

(define-test active-transitions
  (test-state-transitions :active
                          (list '(:cancel-goal :waiting-for-cancel-ack)
                                '(:preempting :preempting)
                                '(:preempted :waiting-for-result)
                                '(:aborted :waiting-for-result)
                                '(:succeeded :waiting-for-result)
                                '(:lost :done)
                                '(:receive :done))))                     

(define-test pending-transitions
  (test-state-transitions :pending
                          (append (list '(:cancel-goal :waiting-for-cancel-ack)
                                        '(:active :active)
                                        '(:preempting :preempting)
                                        '(:recalling :recalling))
                                  *transitions-to-waiting-for-result*)))

(define-test waiting-for-goal-ack-transitions
  (test-state-transitions :waiting-for-goal-ack
                          (append (list '(:preempting :preempting)
                                        '(:recalling :recalling))
                                  *transitions-to-waiting-for-result*)))

(define-test recalling-transitions
  (test-state-transitions :recalling
                          (append (list '(:preempting :preempting))
                                  *transitions-to-waiting-for-result*)))

(define-test preempting-transitions
  (test-state-transitions :preempting
                          (list '(:preempted :waiting-for-result)
                                '(:aborted :waiting-for-result)
                                '(:succeeded :waiting-for-result)
                                '(:lost :done)
                                '(:receive :done))))

(define-test waiting-for-result-transitions
  (test-state-transitions :waiting-for-result
                          (list '(:receive :done)
                                '(:lost :done))))

(define-test done-transitions
  (test-state-transitions :done
                          (list '(:send-goal :waiting-for-goal-ack))))


;;; Tests for incoming updates

(defvar *valid-statuses* '(:pending :active :preempted :succeeded :aborted
                           :rejected :preempting :recalling :recalled :lost))

(defvar *valid-states* '(:done :waiting-for-goal-ack :pending :active
                         :wating-for-cancel-ack :recalling :preempting
                         :wating-for-result))

(defun make-csm (transition-cb feedback-cb send-cancel-fn)
  (make-instance 'actionlib-lisp::comm-state-machine 
                 :goal-id "test-id"
                 :transition-cb transition-cb
                 :feedback-cb feedback-cb
                 :send-cancel-fn send-cancel-fn))

(defun set-state (stm state-name)
  (actionlib-lisp::set-current-state stm state-name))

(defun test-update-status (state status)
  (let* ((transition-received nil)
         (transition-cb #'(lambda () (setf transition-received t)))
         (csm (make-csm transition-cb nil nil))
         (target-state nil))
    (set-state (actionlib-lisp::stm csm) state)
    (setf target-state (actionlib-lisp::get-next-state (actionlib-lisp::stm csm) status))
    (actionlib-lisp::update-status csm status)
    (when target-state
          (assert-true transition-received status state (actionlib-lisp::name target-state))
          (assert-equal (actionlib-lisp::name target-state) 
                        (actionlib-lisp::comm-state csm))
          (assert-equal (actionlib-lisp::latest-goal-status csm) status))))

(define-test update-status
  (loop for status in *valid-statuses*
        do (loop for state in *valid-states*
                 do (test-update-status state status))))

(define-test update-result
  (loop for state in *valid-states*
        do (let* ((transition-received nil)
                  (transition-cb #'(lambda () (setf transition-received t)))
                  (csm (make-csm transition-cb nil nil)))
             (set-state (actionlib-lisp::stm csm) state)
             (assert-false (actionlib-lisp::latest-result csm))
             (actionlib-lisp::update-result csm "test-result")
             (assert-true transition-received)
             (assert-equal (actionlib-lisp::latest-result csm) "test-result")
             (assert-equal (actionlib-lisp::comm-state csm) :done))))

(define-test update-feedback
  (let* ((feedback-received nil)
         (feedback-cb #'(lambda (x) (declare (ignore x))
                          (setf feedback-received t)))
         (csm (make-csm nil feedback-cb nil)))
    (assert-false (actionlib-lisp::latest-feedback csm))
    (actionlib-lisp::update-feedback csm "test-feedback")
    (assert-true feedback-received)
    (assert-equal (actionlib-lisp::latest-feedback csm) "test-feedback")))
  
;;New status received after the result
(define-test update-status-and-result
  (let ((csm (make-csm nil nil nil)))
    (actionlib-lisp::update-result csm "test-result")
    (actionlib-lisp::update-status csm :succeeded)
    (assert-equal (actionlib-lisp::latest-goal-status csm) :succeeded)
    (assert-equal (actionlib-lisp::comm-state csm) :done)))


    
                                  
                                  