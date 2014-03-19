(in-package :actionlib)

(defclass goal-manager ()
  ((id :initform 0)
   (action-type :initarg :action-type
                :accessor action-type)
   (cancel-fn :initarg :cancel-fn
              :reader cancel-fn)
   (send-goal-fn :initarg :send-goal-fn
                 :reader send-goal-fn)
   (statuses :initform nil
             :accessor statuses)))

(defgeneric init-goal (manager goal-msg &optional transition-cb feedback-cb))

(defgeneric update-statuses (manager status-array))

(defgeneric update-results (manager action-results))

(defgeneric update-feedbacks (manager action-feedback))


;;;Implementation

(defun generate-goal-id (manager)
  nil)

(defun get-header (manager)
  nil)

(defun make-action-goal (manager goal-id)
  nil)

(defmethod init-goal ((manager goal-manager) goal-msg &optional
                                                        transition-cb
                                                        feedback-cb)
  (let* ((goal-id (generate-goal-id manger))
         (header (get-header manager))
         (csm (make-instance 'comm-state-machine 
                             :goal-id goal-id
                             :transition-cb transition-cb
                             :feedback-cb feedback-cb
                             :send-goal-fn (send-goal-fn manager)
                             :send-cancel-fn (cancel-fn manager))))
    (funcall (send-goal-fn manager) (make-action-goal manager goal-id))  
    (push csm (statuses manager))
    (make-instance 'client-goal-handler
                   :comm-state-machine csm)))

(defmethod update-statuses ((manager goal-manager) status-array)
  nil)

(defmethod update-results ((manager goal-manager) action-results)
  nil)

(defmethod update-feedbacks ((manager goal-manager) action-feedback)
  nil)
