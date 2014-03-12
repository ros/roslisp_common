(in-package :actionlib-test)

(defvar *valid-transitions* '(:send-goal :cancel-goal :pending :active 
                             :recalling :preempting :rejected :aborted
                             :succeeded :recalled :preempted :receive))

(defvar *transitions-to-waiting-for-result* 
  (list '(:rejected :waiting-for-result)
        '(:recalled :waiting-for-result)
        '(:preempted :waiting-for-result)
        '(:aborted :waiting-for-result)
        '(:succeeded :waiting-for-result)
        '(:receive :done)))

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
  (actionlib::set-state state)
  (assert-equal (second transition) (actionlib::transit (first transition))))


(define-test waiting-for-goal-ack-transitions
  (test-state-transitions :waiting-for-goal-ack
                          (append (list '(:cancel-goal :waiting-for-cancel-ack)
                                        '(:active :active)
                                        '(:pending :pending)
                                        '(:recalling :recalling)
                                        '(:preempting :prempting)
                                        '(:receive :done)) 
                                  *transitions-to-waiting-for-result*)))

(define-test active-transitions
  (test-state-transitions :active
                          (list '(:cancel-goal :waiting-for-cancel-ack)
                                '(:preempting :prempting)
                                '(:preempted :waiting-for-result)
                                '(:aborted :waiting-for-result)
                                '(:succeeded :waiting-for-result)
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
                                '(:receive :done))))

(define-test waiting-for-result-transitions
  (test-state-transitions :waiting-for-result
                          (list '(:receive :done))))

(define-test done-transitions
  (test-state-transitions :done
                          (list '(:send-goal :waiting-for-goal-ack))))
                                  
                                  