(in-package :actionlib-test)

(defvar *status-pub* nil)
(defvar *feedback-pub* nil)
(defvar *result-pub* nil)

(defvar *received-goal* nil)

(defun init-callbacks ()
  (setf *received-goal* nil))

(defun advertise-server-topics ()
  (setf *status-pub* (advertise "testname/status" "actionlib_msgs/GoalStatusArray"))
  (setf *feedback-pub* (advertise "testname/feedback" "actionlib/TestActionFeedback"))
  (setf *result-pub* (advertise "testname/result"  "actionlib/TestActionResult")))

(defun received-goal (msg)
  msg
  (setf *received-goal* t))

(define-test send-goal
  (init-callbacks)
  (let ((client (actionlib::make-action-client "testname" "testtypeAction")))
    (subscribe "testname/goal" "testtypeActionGoal" #'received-goal)
    (sleep 1)
    (actionlib::send-goal client (make-message "actionlib/testActionGoal"))))