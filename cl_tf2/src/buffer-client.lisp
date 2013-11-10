(in-package :cl-tf2)

(defclass buffer-client ()
  ((client :initarg :client :reader client)
   (frequency :initarg :frequency :initform 10 :reader frequency)
   (timeout-padding :initarg :timeout-padding :reader timeout-padding)))