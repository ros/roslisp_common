(in-package :cl-tf2)

(defclass stamped ()
  ((frame-id :initarg :frame-id :reader frame-id :type string)
   (stamp :initarg :stamp :reader stamp :type float)))

(defclass stamped-transform (stamped)
  ((transform :initarg :transform :reader transform)
   (child-frame-id :initarg :child-frame-id :reader child-frame-id :type string)))

(defun make-stamped-transform (frame-id child-frame-id stamp transform)
  (make-instance 'stamped-transform
                 :frame-id frame-id
                 :child-frame-id child-frame-id
                 :stamp stamp
                 :transform transform))