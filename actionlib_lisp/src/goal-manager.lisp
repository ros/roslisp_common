(in-package :actionlib)

(defclass goal-manager ()
  ((goals :initform (make-hash-table :test #'equal)
          :accessor goals)
   (mutex :initform (make-mutex :name "goal-manager-lock")
          :reader manager-mutex))
  (:documentation "asdasd"))

(defgeneric init-goal (manager transition-cb feedback-cb cancel-fn)
  (:documentation "Returns goal id"))

(defgeneric update-statuses (manager status-array))

(defgeneric update-results (manager action-result))

(defgeneric update-feedbacks (manager action-feedback))


;;;Implementation
(defmacro goal-with-id (manager goal-id)
  `(gethash ,goal-id (goals ,manager)))

(defmacro with-id-and-status (status-msg &rest body)
  (let ((hash-value (gensym)))
   `(multiple-value-bind (id status-symbol) (status-msg->id-status ,status-msg)
      (let* ((,hash-value (with-recursive-lock ((manager-mutex manager))
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
         (csm (make-instance 'comm-state-machine 
                             :goal-id goal-id
                             :transition-cb (if transition-cb 
                                                #'(lambda () (funcall transition-cb goal-handle)))
                             :feedback-cb (if feedback-cb
                                              #'(lambda (feedback) 
                                                  (funcall feedback-cb goal-handle feedback)))
                             :send-cancel-fn #'(lambda () (funcall cancel-fn goal-id)))))
    (setf (csm goal-handle) csm)
    (with-recursive-lock ((manager-mutex manager))
      (setf (goal-with-id manager goal-id) csm))
    goal-handle))

(defmethod update-statuses ((manager goal-manager) status-array)
  (with-fields ((status-list status_list)) status-array
    (loop for goal-status being the elements of status-list
          do (with-id-and-status goal-status
               (update-status csm status-symbol)))))

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
