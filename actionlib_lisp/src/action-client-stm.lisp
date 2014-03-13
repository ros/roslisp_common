(in-package :actionlib)

(defclass action-client-stm (state-machine)
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
                             (:waiting-for-result (:receive :done)))))))

                             
