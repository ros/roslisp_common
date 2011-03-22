
(in-package :cl-tf)

(defclass stamped ()
  ((frame-id :initarg :frame-id :reader frame-id :type string)
   (stamp :initarg :stamp :reader stamp :type float)))

(defclass stamped-transform (transform stamped)
  ((child-frame-id :initarg :child-frame-id :reader child-frame-id :type string)))

(defclass pose-stamped (pose stamped) ())

(defclass point-stamped (3d-vector stamped) ())

(defun make-pose-stamped (frame-id stamp translation rotation)
  (make-instance 'pose-stamped
                 :frame-id frame-id
                 :stamp stamp
                 :origin translation
                 :orientation rotation))

(defun make-point-stamped (frame-id stamp 3d-vector)
  (make-instance 'point-stamped
                 :frame-id frame-id
                 :stamp stamp
                 :x (x 3d-vector)
                 :y (y 3d-vector)
                 :z (z 3d-vector)))

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

(defun msg->pose-stamped (msg)
  (with-fields ((frame-id (frame_id header))
                (stamp (stamp header))
                (x (x position pose))
                (y (y position pose))
                (z (z position pose))
                (ax (x orientation pose))
                (ay (y orientation pose))
                (az (z orientation pose))
                (aw (w orientation pose)))
      msg
    (make-pose-stamped
     frame-id stamp
     (make-3d-vector x y z)
     (make-quaternion ax ay az aw))))

(defun msg->pose (msg)
  (with-fields ((x (x position))
                (y (y position))
                (z (z position))
                (ax (x orientation))
                (ay (y orientation))
                (az (z orientation))
                (aw (w orientation)))
      msg
    (make-pose
     (make-3d-vector x y z)
     (make-quaternion ax ay az aw))))

(defun pose-stamped->msg (pose)
  (make-message
   "geometry_msgs/PoseStamped"
   (frame_id header) (frame-id pose)
   (stamp header) (stamp pose)
   (x position pose) (x (origin pose))
   (y position pose) (y (origin pose))
   (z position pose) (z (origin pose))
   (x orientation pose) (x (orientation pose))
   (y orientation pose) (y (orientation pose))
   (z orientation pose) (z (orientation pose))
   (w orientation pose) (w (orientation pose))))

(defun pose->stamped-msg (frame-id stamp pose)
  (make-message
   "geometry_msgs/PoseStamped"
   (frame_id header) frame-id
   (stamp header) stamp
   (x position pose) (x (origin pose))
   (y position pose) (y (origin pose))
   (z position pose) (z (origin pose))
   (x orientation pose) (x (orientation pose))
   (y orientation pose) (y (orientation pose))
   (z orientation pose) (z (orientation pose))
   (w orientation pose) (w (orientation pose))))

(defun pose->msg (pose)
  (make-message
   "geometry_msgs/Pose"
   (x position) (x (origin pose))
   (y position) (y (origin pose))
   (z position) (z (origin pose))
   (x orientation) (x (orientation pose))
   (y orientation) (y (orientation pose))
   (z orientation) (z (orientation pose))
   (w orientation) (w (orientation pose))))
