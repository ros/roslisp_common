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

(defclass comm-state-machine ()
  ((stm :initform (make-instance 'state-machine 
                                 :current-state (getf *states* :waiting-for-goal-ack)
                                 :states *states*)
        :accessor stm)
   (goal-id :initarg :goal-id
            :reader get-goal-id)
   (start-time :initform (ros-time)
               :accessor start-time)
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
                    :accessor latest-feedback)
   (stm-mutex :initform (make-mutex :name "state-machine-lock")
              :reader stm-mutex)
   (stat-mutex :initform (make-mutex :name "status-lock")
                 :reader status-mutex)
   (res-mutex :initform (make-mutex :name "result-lock")
                 :reader result-mutex)
   (fb-mutex :initform (make-mutex :name "feedback-lock")
                   :reader feedback-mutex))
  (:documentation "Monitors the state of the communication between action-client
                   and the server for one goal and executes the callbacks."))

(defgeneric transition-to (csm signal)
  (:documentation "Processes the signal and executes the transition-callback if
                   necessary"))

(defgeneric update-status (csm status)
  (:documentation "Updates the state with the given status."))

(defgeneric update-result (csm action-result)
  (:documentation "Updates the state with the given result."))

(defgeneric update-feedback (csm action-feedback)
  (:documentation "Updates the state with the given feedback and executes the
                   feedback callback."))

(defgeneric comm-state (goal-handle)
  (:documentation "Returns the state of the goal's communication
                   state machine."))

;;; Implementation

(defmethod transition-to ((csm comm-state-machine) signal)
   ;; If the result came before the last status update
  (if (eql (name (get-current-state (stm csm))) :done)
      (if (transition-cb csm)
          (funcall (transition-cb csm))))
  (if (process-signal (stm csm) signal)
      (if (transition-cb csm)
          (funcall (transition-cb csm)))))

(defmethod update-status ((csm comm-state-machine) status)
  (when (not (eql (latest-goal-status csm) status))
    (with-recursive-lock ((status-mutex csm))
      (setf (latest-goal-status csm) status))
    (if (get-next-state (stm csm) status)
        (transition-to csm status))))
      
(defmethod update-result ((csm comm-state-machine) action-result)
  (with-mutex ((result-mutex csm))
    (setf (latest-result csm) action-result))
  (transition-to csm :receive))

(defmethod update-feedback ((csm comm-state-machine) action-feedback)
  (with-mutex ((feedback-mutex csm))
    (setf (latest-feedback csm) action-feedback))
  (if (feedback-cb csm)
      (funcall (feedback-cb csm) action-feedback)))

(defmethod comm-state ((csm comm-state-machine))
  (name (get-current-state (stm csm))))
  
