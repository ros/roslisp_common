
(in-package :cl-tf)

(defclass stamped ()
  ((frame-id :initarg :frame-id :reader frame-id :type string)
   (stamp :initarg :stamp :reader stamp :type float)))

(defclass stamped-transform (transform stamped)
  ((child-frame-id :initarg :child-frame-id :reader child-frame-id :type string)))

(defclass pose-stamped (pose stamped) ())

(defclass point-stamped (3d-vector stamped) ())

(defun make-stamped-transform (frame-id child-frame-id stamp translation rotation)
  (make-instance 'stamped-transform
                 :frame-id frame-id
                 :child-frame-id child-frame-id
                 :stamp stamp
                 :translation translation
                 :rotation rotation))

(defun transform->stamped-transform (frame-id child-frame-id stamp transform)
  (make-instance 'stamped-transform
                 :frame-id frame-id
                 :child-frame-id child-frame-id
                 :stamp stamp
                 :translation (translation transform)
                 :rotation (rotation transform)))

(defun tf-transform->transform (msg)
  (with-fields ((translation translation)
                (rotation rotation))
      msg
    (make-transform
     (with-fields ((x x)
                   (y y)
                   (z z))
         translation
       (make-3d-vector x y z))
     (with-fields ((x x)
                   (y y)
                   (z z)
                   (w w))
         rotation
       (make-quaternion x y z w)))))

(defun tf-message->transforms (tf-msgs)
  "Return the transform that corresponds to a tf message."
  (loop for msg across (transforms-val tf-msgs)
        collecting
     (with-fields ((frame-id (frame_id header))
                   (stamp (stamp header))
                   (child-frame-id child_frame_id))
         msg
       (let ((transform (tf-transform->transform
                         (geometry_msgs-msg:transform-val msg))))
         (make-stamped-transform frame-id child-frame-id stamp
                                 (translation transform)
                                 (rotation transform))))))
