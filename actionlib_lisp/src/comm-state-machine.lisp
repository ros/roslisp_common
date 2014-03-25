(in-package :actionlib)

(defparameter *states* 
  ;;              State   Signal     Target-State
  (make-states '((:done (:send-goal :waiting-for-goal-ack))
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
                                         :receive :done
                                         :lost :done))
                 (:pending (:cancel-goal :waiting-for-cancel-ack
                            :active :active
                            :recalling :recalling
                            :preempting :preempting
                            :rejected :waiting-for-result
                            :recalled :waiting-for-result
                            :preempted :waiting-for-result
                            :succeeded :waiting-for-result
                            :aborted :waiting-for-result
                            :receive :done
                            :lost :done))
                 (:active (:cancel-goal :waiting-for-cancel-ack
                           :preempting :preempting
                           :preempted :waiting-for-result
                           :succeeded :waiting-for-result
                           :aborted :waiting-for-result
                           :receive :done
                           :lost :done))
                 (:waiting-for-cancel-ack (:recalling :recalling
                                           :preempting :preempting
                                           :rejected :waiting-for-result
                                           :recalled :waiting-for-result
                                           :preempted :waiting-for-result
                                           :succeeded :waiting-for-result
                                           :aborted :waiting-for-result
                                           :receive :done
                                           :lost :done))
                 (:recalling (:preempting :preempting
                              :rejected :waiting-for-result
                              :recalled :waiting-for-result
                              :preempted :waiting-for-result
                              :succeeded :waiting-for-result
                              :aborted :waiting-for-result
                              :receive :done
                              :lost :done))
                 (:preempting (:preempted :waiting-for-result
                               :succeeded :waiting-for-result
                               :aborted :waiting-for-result
                               :receive :done
                               :lost :done))
                 (:waiting-for-result (:receive :done
                                       :lost :done)))))

(defclass comm-state-machine (state-machine)
  ((current-state :initform (getf *states* :waiting-for-goal-ack))
   (states :initform *states*)
   (goal-id :initarg :goal-id
            :reader get-goal-id
            :documentation "lala")
   (transition-cb :initarg :transition-cb
                  :initform nil
                  :accessor transition-cb)
   (feedback-cb :initarg :feedback-cb
                :initform nil
                :accessor feedback-cb)
   (send-goal-fn :initarg :send-goal-fn
                 :reader send-goal-fn)
   (send-cancel-fn :initarg :send-cancel-fn
                   :reader send-cancel-fn)
   (latest-goal-status :initform :waiting-for-goal-ack
                       :accessor latest-goal-status)
   (latest-result :initform nil
                  :accessor latest-result)
   (latest-feedback :initform nil
                    :accessor latest-feedback))
   (:documentation "lala"))

(defgeneric transition-to (csm signal))

(defgeneric update-status (csm status))

(defgeneric update-result (csm action-result))

(defgeneric update-feedback (csm action-feedback))


;;;Implementation

(defmethod transition-to ((csm comm-state-machine) signal)
  (if (process-signal csm signal)
      (if (transition-cb csm)
          (funcall (transition-cb csm)))))

(defmethod update-status ((csm comm-state-machine) status)
  (when (not (eql (latest-goal-status csm) status))
    (setf (latest-goal-status csm) status)  
    ;; If the result came before the last status update
    (if (eql (name (current-state csm)) :done)
        (if (transition-cb csm)
          (funcall (transition-cb csm))))
    (if (get-next-state csm status)
        (transition-to csm status))))
      
(defmethod update-result ((csm comm-state-machine) action-result)
  (transition-to csm :receive)
  (setf (latest-result csm) action-result))

(defmethod update-feedback ((csm comm-state-machine) action-feedback)
  (setf (latest-feedback csm) action-feedback)
  (if (feedback-cb csm)
      (funcall (feedback-cb csm))))
