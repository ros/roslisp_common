(in-package :actionlib)

(defclass goal-manager ()
  ((goal-id :initform 0)
   (cancel-fn :initarg :cancel-fn)
   (send-goal-fn :initarg :send-goal-fn)
   (statuses :initform nil
             :accessor statuses)))

(defgeneric init-goal (manager goal &optional transition-cb feedback-cb))

(defgeneric update-statuses (manager status-array))

(defgeneric update-results (manager action-results))

(defgeneric update-feedback (manager action-feedback))


;;;Implementation

(defmethod init-goal ((manager goal-manager) goal &optional
                                                    transition-cb
                                                    feedback-cb)
  nil)

(defmethod update-statuses ((manager goal-manager) status-array)
  nil)

(defmethod update-results ((manager goal-manager) action-results)
  nil)

(defmethod update-feedback ((manager goal-manager) action-feedback)
  nil)
