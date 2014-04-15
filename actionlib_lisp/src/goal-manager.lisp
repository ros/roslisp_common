(in-package :actionlib-lisp)

(defclass goal-manager ()
  ((goals :initform (make-hash-table :test #'equal)
          :accessor goals
          :documentation "Hashtable with all comm-state-machines of all
                          monitored goals.")
   (goal-ids :initform nil
             :accessor goal-ids
             :documentation "List of all goal-ids of all monitored goals")
   (waiting-for-goal-ack-timeout :initform 2
                                 :accessor waiting-for-goal-ack-timeout
                                 :documentation "Time in seconds to wait for server
                                                 to acknowledge the goal before
                                                 goal-status is set to lost.")
   (csm-type :initform 'comm-state-machine
             :initarg :csm-type
             :reader csm-type)
   (hash-mutex :initform (make-mutex :name "goal-hash-table-lock")
               :reader hash-mutex)
   (id-mutex :initform (make-mutex :name "goal-ids-lock")
             :reader id-mutex))
  (:documentation "Contains all comm-state-machines and updates them if new messages arrive."))

(defgeneric init-goal (manager transition-cb feedback-cb cancel-fn)
  (:documentation "Initializes a new goal and returns the goal-handle."))

(defgeneric update-statuses (manager status-array)
  (:documentation "Reads the information from the status-array and updates the
                   comm-state-machines accordingly."))

(defgeneric update-results (manager action-result)
  (:documentation "Updates the with the action-result associated comm-state-machine
                   with the result message."))

(defgeneric update-feedbacks (manager action-feedback)
  (:documentation "Updates the with the action-feedback associated comm-state-machine
                   with the feedback message."))


;;;Implementation
(defun status-msg->id-status (status-msg)
  "Gets a status msg and returns a the id and status"
  (with-fields (status (id (id goal_id))) status-msg
    (let ((status-symbol (car (rassoc status
                                      (symbol-codes 'actionlib_msgs-msg:GoalStatus)))))
      (values id status-symbol))))

(defun generate-goal-id ()
  "Generates a new goal-id"
  ;; TODO(Jannik): chose something standard-compliant here
  (format nil "a_lisp_~a_~a" *ros-node-name* (ros-time)))

(defmethod init-goal ((manager goal-manager) transition-cb feedback-cb cancel-fn)
  "Creates a new comm-state-machine and goal-handle and returns the goal-handle"
  (let* ((goal-id (generate-goal-id))
         (goal-handle (make-instance 'client-goal-handle))
         (csm (make-instance (csm-type manager)
                             :goal-id goal-id
                             :transition-cb (if transition-cb 
                                                #'(lambda () (funcall transition-cb goal-handle)))
                             :feedback-cb (if feedback-cb
                                              #'(lambda (feedback) 
                                                  (funcall feedback-cb goal-handle feedback)))
                             :send-cancel-fn #'(lambda () (funcall cancel-fn goal-id)))))
    (setf (csm goal-handle) csm)
    (with-recursive-lock ((hash-mutex manager))
      (setf (gethash goal-id (goals manager)) csm))
    (with-recursive-lock ((id-mutex manager))
      (push goal-id (goal-ids manager)))
    goal-handle))

(defmethod update-statuses ((manager goal-manager) status-array)
  "Updates the statuses of all goals that the goal-manager tracks. If the status 
   array contains the goal-id of comm-state-machine, the state of the comm-state-machine
   gets updated with the status else the comm-state-machine gets set to lost"
  (let ((goal-ids (with-recursive-lock ((id-mutex manager))
                    (copy-list (goal-ids manager)))))
    (with-fields ((status-list status_list)) status-array
      (loop for goal-status being the elements of status-list
            do (multiple-value-bind (id status-symbol) (status-msg->id-status goal-status)
                 (multiple-value-bind (comm-state-machine has-state-machine-p) (gethash id (goals manager))
                   (when has-state-machine-p
                     (setf goal-ids (remove id goal-ids :test #'equal))
                     (update-status comm-state-machine status-symbol)))))
      (loop for goal-id in goal-ids
            do (let ((csm (with-recursive-lock ((hash-mutex manager))
                            (nth-value 0 (gethash ,goal-id (goals ,manager))))))
                 (when (or (not (eql (name (get-current-state (stm csm)))
                                     :waiting-for-goal-ack))
                           (> (- (ros-time) (start-time csm)) 
                              (waiting-for-goal-ack-timeout manager)))
                   (with-recursive-lock ((id-mutex manager))
                     (setf (goal-ids manager) 
                           (remove goal-id (goal-ids manager) :test #'equal)))
                   (update-status csm :lost)))))))

(defmethod update-results ((manager goal-manager) action-result)
  "Updates the comm-state-machine with the goal-id from the result message."
  (with-fields (status result) action-result
    (multiple-value-bind (id status-symbol) (status-msg->id-status status)
      (multiple-value-bind (comm-state-machine has-state-machine-p) (gethash id (goals manager))
        (when has-state-machine-p
          (update-status csm status-symbol)
          (update-result csm result))))))
    
(defmethod update-feedbacks ((manager goal-manager) action-feedback)
  "Updates the comm-state-machine with the goal-id from the feedback message."
  (with-fields (status feedback) action-feedback
    (multiple-value-bind (id status-symbol) (status-msg->id-status status)
      (multiple-value-bind (comm-state-machine has-state-machine-p) (gethash id (goals manager))
        (when has-state-machine-p
          (update-status csm status-symbol)
          (update-feedback csm feedback))))))

(defmethod cancel-all-goals ((manager goal-manager))
  
