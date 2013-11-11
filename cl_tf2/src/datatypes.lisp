(in-package :cl-tf2)

(defclass header ()
  ((frame-id :initarg :frame-id :reader frame-id :type string)
   (stamp :initarg :stamp :reader stamp :type float)))

(defclass stamped-transform ()
  ((header :initarg :header :reader header)
   (transform :initarg :transform :reader transform)
   (child-frame-id :initarg :child-frame-id :reader child-frame-id :type string)))

(defun make-stamped-transform (frame-id child-frame-id stamp transform)
  (make-instance 'stamped-transform
                 :header (make-instance 'header
                                        :frame-id frame-id
                                        :stamp stamp)
                 :child-frame-id child-frame-id
                 :transform transform))