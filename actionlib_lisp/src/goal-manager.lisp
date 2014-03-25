(in-package :actionlib)

(defclass goal-manager ()
  ((last-id :initform 0
            :accessor last-id)
   (cancel-fn :initarg :cancel-fn
              :reader cancel-fn)
   (goals :initform nil
          :accessor goals)))

(defgeneric init-goal (manager goal-msg &optional transition-cb feedback-cb)
  (:documentation "Returns goal id"))

(defgeneric update-statuses (manager status-array))

(defgeneric update-results (manager action-results))

(defgeneric update-feedbacks (manager action-feedback))


;;;Implementation

(defun generate-goal-id (manager)
  (format nil "actionlib_lisp~a" (incf (last-id manager))))

(defmethod init-goal ((manager goal-manager) goal-msg &optional
                                                        transition-cb
                                                        feedback-cb)
  (let* ((goal-id (generate-goal-id manager))
         (goal-handle (make-instance 'client-goal-handle))
         (csm (make-instance 'comm-state-machine 
                             :goal-id goal-id
                             :send-cancel-fn (cancel-fn manager)
                             :transition-cb #'(lambda () (funcall transition-cb goal-handle))
                             :feedback-cb #'(lambda () (funcall feedback-cb goal-handle))
                             :send-cancel-fn #'(lambda () (funcall (cancel-fn manager) goal-id)))))
    (setf (csm goal-handle) csm)
    (push csm (goals manager))
    goal-handle))

(defmethod update-statuses ((manager goal-manager) status-array)
  (with-fields ((status-list status_list)) status-array
    (loop for goal-status being the elements of status-list
          do (with-fields (status (id (id goal_id))) goal-status
               ;;suche csm mit goal-id id
               ;;update-status
               ))))

(defmethod update-results ((manager goal-manager) action-results)
  nil)

(defmethod update-feedbacks ((manager goal-manager) action-feedback)
  nil)
