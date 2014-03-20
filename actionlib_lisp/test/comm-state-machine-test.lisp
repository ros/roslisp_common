(in-package :actionlib-test)


;;;;;; TODO ADD TRANSITIONS IF GOAL IS LOST

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
  (let ((stm (make-instance 'actionlib::comm-state-machine))
        (target-state nil)
        (target-state-name nil))
    (actionlib::set-current-state stm state)
    (setf target-state (actionlib::process-signal stm (first transition)))
    (if target-state
        (setf target-state-name (actionlib::name target-state)))
    (assert-equal (second transition) target-state-name)))


;;; Test for the state machine

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


;;; Tests for incoming updates

(defvar *valid-statuses* '(:pending :active :preempted :succeeded :aborted
                           :rejected :preempting :recalling :recalled :lost))

(defvar *valid-states* '(:done :waiting-for-goal-ack :pending :active
                         :wating-for-cancel-ack :recalling :preempting
                         :wating-for-result))

(defparameter *transition-cb-value* nil)

(defparameter *feedback-cb-value* nil)

(defparameter *send-goal-fn-value* nil)

(defparameter *send-cancel-fn-value* nil)

(defun make-csm ()
  (make-instance 'actionlib::comm-state-machine 
                 :goal-id "test-id"
                 :transition-cb #'test-transition-cb
                 :feedback-cb #'test-feedback-cb
                 :send-goal-fn #'test-send-goal-fn
                 :send-cancel-fn #'test-send-cancel-fn))

(defun test-transition-cb ()
  (setf *transition-cb-value* t))

(defun test-feedback-cb ()
  (setf *feedback-cb-value* t))

(defun test-send-goal-fn ()
  (setf *send-goal-fn-value* t))

(defun test-send-cancel-fn ()
  (setf *send-cancel-fn-value* t))

(defun init-callbacks ()
  (setf *transition-cb-value* nil)
  (setf *feedback-cb-value* nil)
  (setf *send-goal-fn-value* nil)
  (setf *send-cancel-fn-value* nil))

(defun set-state (csm state-name)
  (actionlib::set-current-state csm (actionlib::get-state csm state-name)))

(defun test-update-status (state status)
  (let* ((csm (make-csm))
         (target-state (actionlib::get-next-state csm status)))
    (init-callbacks)
    (set-state csm state)
    (actionlib::update-status csm status)
    (assert-true *transition-cb-value*)
    (assert-equal (actionlib::get-state csm) target-state)
    (assert-equal (actionlib::latest-goal-status csm) status)))

(define-test update-status
  (loop for status in *valid-statuses*
        do (loop for state in *valid-states*
                 do (test-update-status state status))))

(define-test update-result
  (loop for state in *valid-states*
        do (let ((csm (make-csm)))
             (init-callbacks)
             (set-state csm state)
             (assert-false (actionlib::latest-result csm))
             (actionlib::update-result csm "test-result")
             (assert-true *transition-cb-value*)
             (assert-equal (actionlib::latest-result csm) "test-result")
             (assert-equal (actionlib::name (actionlib::get-state csm)) :done))))

(define-test update-feedback
  (let ((csm (make-csm)))
    (init-callbacks)
    (assert-false (actionlib::latest-feedback csm))
    (actionlib::update-feedback csm "test-feedback")
    (assert-true *feedback-cb-value*)
    (assert-equal (actionlib::latest-feedback csm) "test-feedback")))
  
;;New status received after the result
(define-test update-status-and-result
  (let ((csm (make-csm)))
    (actionlib::update-result csm "test-result")
    (actionlib::update-status csm :succeeded)
    (assert-equal (actionlib::latest-goal-status csm) :succeeded)
    (assert-equal (actionlib::name (actionlib::get-state csm)) :done)))


    
                                  
                                  