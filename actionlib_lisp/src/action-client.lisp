(in-package :actionlib)

(defclass action-client ()
  ((state-machine
    :initform (make-instance 'action-client-stm)
    :accessor state-machine)))