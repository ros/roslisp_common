(in-package :actionlib)

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
(defmacro goal-with-id (manager goal-id)
  `(gethash ,goal-id (goals ,manager)))

(defmacro with-id-and-status (status-msg &rest body)
  (let ((hash-value (gensym)))
   `(multiple-value-bind (id status-symbol) (status-msg->id-status ,status-msg)
      (let* ((,hash-value (with-recursive-lock ((hash-mutex manager))
                            (goal-with-id manager id)))
             (csm (nth-value 0 ,hash-value)))
        (when ,hash-value
          ,@body)))))

(defun status-msg->id-status (status-msg)
  (with-fields (status (id (id goal_id))) status-msg
    (let ((status-symbol (car (rassoc status
                                      (symbol-codes 'actionlib_msgs-msg:GoalStatus)))))
      (values id status-symbol))))

(defun generate-goal-id ()
  (format nil "a_lisp_~a_~a" *ros-node-name* (ros-time)))

(defmethod init-goal ((manager goal-manager) transition-cb feedback-cb cancel-fn)
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
      (setf (goal-with-id manager goal-id) csm))
    (with-recursive-lock ((id-mutex manager))
      (push goal-id (goal-ids manager)))
    goal-handle))

(defmethod update-statuses ((manager goal-manager) status-array)
  (let ((goal-ids (with-recursive-lock ((id-mutex manager))
                    (copy-list (goal-ids manager)))))
    (with-fields ((status-list status_list)) status-array
      (loop for goal-status being the elements of status-list
            do (with-id-and-status goal-status
                 (setf goal-ids (remove id goal-ids :test #'equal))
                 (update-status csm status-symbol)))
      (loop for goal-id in goal-ids
            do (let ((csm (with-recursive-lock ((hash-mutex manager))
                            (nth-value 0 (goal-with-id manager goal-id)))))
                 (when (or (not (eql (name (get-current-state (stm csm)))
                                     :waiting-for-goal-ack))
                           (> (- (ros-time) (start-time csm)) 
                              (waiting-for-goal-ack-timeout manager)))
                   (with-recursive-lock ((id-mutex manager))
                     (setf (goal-ids manager) 
                           (remove goal-id (goal-ids manager) :test #'equal)))
                   (update-status csm :lost)))))))

(defmethod update-results ((manager goal-manager) action-result)
  (with-fields (status result) action-result
    (with-id-and-status status
      (update-status csm status-symbol)
      (update-result csm result))))

(defmethod update-feedbacks ((manager goal-manager) action-feedback)
  (with-fields (status feedback) action-feedback
    (with-id-and-status status
      (update-status csm status-symbol)
      (update-feedback csm feedback))))
