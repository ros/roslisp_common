(in-package :actionlib-test-server)

(defclass action-server ()
  ((goals :initform nil
          :accessor goals)
   (seq :initform 0
        :accessor seq)
   (publish-status :initform t
                   :accessor publish-status)
   (feedback-pub :initform (advertise "/fibonacci/feedback"
                                      "actionlib_tutorials/FibonacciActionFeedback")
                 :accessor feedback-pub)
   (status-pub :initform (advertise "/fibonacci/status"
                                   "actionlib_msgs/GoalStatusArray")
               :accessor status-pub)
   (result-pub :initform (advertise "/fibonacci/result"
                                    "actionlib_tutorials/FibonacciActionResult")
               :accessor result-pub)
   (mutex :initform (make-mutex :name "action-server-lock")
          :reader server-mutex)))

(defclass goal ()
  ((goal-id :initarg :goal-id
            :reader goal-id)
   (order :initarg :order
          :accessor order)
   (status :initform :pending
           :accessor status)
   (mutex :initform (make-mutex)
          :reader goal-mutex)))

(defun get-goal (server goal-id)
  (with-recursive-lock ((server-mutex server))
    (find goal-id (goals server) :test #'(lambda (id goal) (equal id (goal-id goal))))))

(defun make-status-list (server)
  (let ((status-list (make-array 5 :fill-pointer 0))
        (goals nil))
    (with-recursive-lock ((server-mutex server))
      (setf goals (copy-list (goals server))))
    (loop for goal in goals
          do (with-recursive-lock ((goal-mutex goal))
               (vector-push (make-message "actionlib_msgs/GoalStatus"
                                   status (symbol-code 'actionlib_msgs-msg:GoalStatus 
                                                       (status goal))
                                   (id goal_id) (goal-id goal)
                                   (stamp goal_id) (ros-time))
                            status-list)))
    status-list))

(defun send-status-msg (server)
  (let ((status-list (make-status-list server)))
    ;(format t "status-list: ~a" status-list)
    (publish (status-pub server) 
             (make-message "actionlib_msgs/GoalStatusArray"
                           (seq header) (incf (seq server))
                           (stamp header) (ros-time)
                           status_list status-list))))    

(defun start-status-thread (server)
  (sb-thread:make-thread (lambda () 
                           (loop while t
                                 do (if (publish-status server)
                                        (send-status-msg server))
                                    (sleep 0.5)))))

(defun goal-callback (server msg)
  (with-fields ((goal-id (id goal_id)) (order (order goal))) msg
    (format t "goal-id: ~a~%order: ~a~%" goal-id order)
    (let ((goal (make-instance 'goal
                               :goal-id goal-id
                               :order order))) 
      (format t "Made Goal~%")
      (with-recursive-lock ((server-mutex server))
        (push goal (goals server))))))

(defun cancel (server goal)
  (format t "Cancel goal: ~a~%" (goal-id goal))
  (with-recursive-lock ((goal-mutex goal))
    (if (eql (status goal) :pending)
        (setf (status goal) :recalling))
    (if (eql (status goal) :active)
        (setf (status goal) :preempting)))
  (sb-thread:make-thread (lambda ()
                           (sleep 1)
                           (when (eql (status goal) :recalling)
                             (recalling-goal server (goal-id goal))
                             (sleep 1)
                             (recalled-goal server (goal-id goal)))
                           (when (eql (status goal) :preempting)
                             (preempting-goal server (goal-id goal))
                             (sleep 1)
                             (preempted-goal server (goal-id goal))))))

(defun cancel-callback (server msg)
  (with-fields ((goal-id id)) msg
    (if (equal goal-id "")
        (loop for goal in (goals server)
              do (cancel server goal))
        (cancel server (get-goal server goal-id)))))

(defun make-action-server ()
  (let ((action-server (make-instance 'action-server)))
    (subscribe "fibonacci/goal" "actionlib_tutorials/FibonacciActionGoal"
               #'(lambda (msg) (goal-callback action-server msg)))
    (subscribe "fibonacci/cancel" "actionlib_msgs/GoalID"
               #'(lambda (msg) (cancel-callback action-server msg)))
    (start-status-thread action-server)
    action-server))




(defun lose-goal (server goal-id)
  (with-recursive-lock ((server-mutex server))
    (setf (goals server) 
          (remove-if #'(lambda (goal) (equal goal-id (goal-id goal)))
                     (goals server)))))

(defun activate-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :active))))

(defun abort-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :aborted))
    (send-result server goal-id :aborted)))

(defun finish-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :succeeded))
    (send-result server goal-id :succeeded)))

(defun recalled-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :recalled))
    (send-result server goal-id :recalled)))

(defun recalling-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :recalling))))

(defun preempted-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :preempted))
    (send-result server goal-id :preempted)))

(defun preempting-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :preempting))))

(defun reject-goal (server goal-id)
  (let ((goal (get-goal server goal-id)))
    (with-recursive-lock ((goal-mutex goal))
      (setf (status goal) :rejected))
    (send-result server goal-id :rejected)))

(defun send-result (server goal-id status)
  (publish (result-pub server) (make-message "actionlib_tutorials/FibonacciActionResult"
                                             (seq header) 101
                                             (stamp header) (ros-time)
                                             (status status) (symbol-code 'actionlib_msgs-msg:GoalStatus 
                                                                          status)
                                             (id goal_id status) goal-id
                                             (stamp goal_id status) (ros-time))))


  
   