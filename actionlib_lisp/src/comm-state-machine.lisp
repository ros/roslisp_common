(in-package :actionlib)

(defclass comm-state-machine (state-machine)
  ((current-state
    :initform (make-instance 'state 
                             :name :done
                             :transitions '(:send-goal :waiting-for-goal-ack)))
   (states
    ;;                        State   Signal     Target-State
    :initform (make-states '((:done (:send-goal :waiting-for-goal-ack))
                             (:waiting-for-goal-ack (:cancel-goal :waiting-for-cancel-ack
                                                     :pending :pending
                                                     :active :active
                                                     :recalling :recalling
                                                     :preempting :preempting
                                                     :rejected :waiting-for-result
                                                     :recalled :waiting-for-result
                                                     :preempted :waiting-for-result
                                                     :succeeded :waiting-for-result
                                                     :aborted :waiting-for-result
                                                     :receive :done))
                             (:pending (:cancel-goal :waiting-for-cancel-ack
                                        :active :active
                                        :recalling :recalling
                                        :preempting :preempting
                                        :rejected :waiting-for-result
                                        :recalled :waiting-for-result
                                        :preempted :waiting-for-result
                                        :succeeded :waiting-for-result
                                        :aborted :waiting-for-result
                                        :receive :done))
                             (:active (:cancel-goal :waiting-for-cancel-ack
                                       :preempting :preempting
                                       :preempted :waiting-for-result
                                       :succeeded :waiting-for-result
                                       :aborted :waiting-for-result
                                       :receive :done))
                             (:waiting-for-cancel-ack (:recalling :recalling
                                                       :preempting :preempting
                                                       :rejected :waiting-for-result
                                                       :recalled :waiting-for-result
                                                       :preempted :waiting-for-result
                                                       :succeeded :waiting-for-result
                                                       :aborted :waiting-for-result
                                                       :receive :done))
                             (:recalling (:preempting :preempting
                                          :rejected :waiting-for-result
                                          :recalled :waiting-for-result
                                          :preempted :waiting-for-result
                                          :succeeded :waiting-for-result
                                          :aborted :waiting-for-result
                                          :receive :done))
                             (:preempting (:preempted :waiting-for-result
                                           :succeeded :waiting-for-result
                                           :aborted :waiting-for-result
                                           :receive :done))
                             (:waiting-for-result (:receive :done)))))
   (goal :initarg :goal)
   (transition-cb :initarg :transition-cb
                  :initform nil
                  :reader transition-cb)
   (feedback-cb :initarg :feedback-cb
                :initform nil
                :reader :feedback-cb)
   (send-goal-fn :initarg :send-goal-fn
                 :reader send-goal-fn)
   (send-cancel-fn :initarg :send-cancel-fn
                   :reader send-cancel-fn)
   (latest-goal-status :initform nil
                       :accessor latest-goal-status)
   (latest-result :initform nil
                  :accessor latest-result)))

(defgeneric update-status (csm state-name))

(defgeneric update-result (csm action-result))

(defgeneric update-feedback (csm action-feedback))


;;;Implementation

(defmethod process-signal ((csm comm-state-machine) signal)
  nil
  (call-next-method))

(defmethod update-status ((csm comm-state-machine) state-name)
  nil)

(defmethod update-result ((csm comm-state-machine) action-result)
  nil)

(defmethod update-feedback ((csm comm-state-machine)action-feedback)
  nil)
