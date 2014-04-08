(in-package :cl-user)

(defpackage :actionlib
  (:use :cl :roslisp :actionlib_msgs-msg :sb-thread)
  (:export :cancel :cancel-all-goals :cancel-at-before-time :comm-state
           :goal-id :goal-status :is-connected :make-action-goal
           :result :send-goal :terminal-state :wait-for-server))
           