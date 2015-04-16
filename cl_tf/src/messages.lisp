
(in-package :cl-tf)

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

(defun transforms->tf (transforms)
  (make-message "tf/tfMessage" :transforms (map 'vector #'transform->msg transforms)))

(defun transform->tf (tr)
  (make-message "tf/tfMessage" :transforms (vector (transform->msg tr))))

(defun transform->msg (tr)
  (with-slots (frame-id child-frame-id stamp translation rotation) tr
   (make-message "geometry_msgs/TransformStamped"
                 (:frame_id :header) frame-id
                 (:stamp :header) stamp
                 :child_frame_id child-frame-id
                 (:translation :transform) (to-vector3 translation)
                 (:rotation :transform) (to-quaternion-msg rotation))))

(defun tf-message->transforms (tf-msgs)
  "Return the transform that corresponds to a tf message."
  (loop for msg across (transforms tf-msgs)
        collecting
     (with-fields ((frame-id (frame_id header))
                   (stamp (stamp header))
                   (child-frame-id child_frame_id))
         msg
       (let ((transform (tf-transform->transform
                         (geometry_msgs-msg:transform msg))))
         (cl-tf-datatypes:make-transform-stamped
          frame-id child-frame-id stamp
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

(defun to-vector3 (trans)
  (make-msg "geometry_msgs/Vector3" :x (x trans) :y (y trans) :z (z trans)))

(defun to-quaternion-msg (rot)
  (make-msg "geometry_msgs/Quaternion" :x (x rot) :y (y rot) :z (z rot) :w (w rot)))

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

(defun msg->point-stamped (point-stamped-message)
  (declare (type geometry_msgs-msg:pointstamped
                 point-stamped-message))
  (with-fields ((frame-id (frame_id header))
                (stamp (stamp header))
                (x (x point))
                (y (y point))
                (z (z point)))
      point-stamped-message
    (make-point-stamped
     frame-id stamp
     (cl-transforms:make-3d-vector x y z))))

(defun point-stamped->msg (point-stamped)
  (declare (type point-stamped point-stamped))
  (make-message
   "geometry_msgs/PointStamped"
   (stamp header) (stamp point-stamped)
   (frame_id header) (frame-id point-stamped)
   (x point) (x point-stamped)
   (y point) (y point-stamped)
   (z point) (z point-stamped)))

(defun msg->point (point-message)
  (declare (type geometry_msgs-msg:point point-message))
  (with-fields (x y z) point-message
    (make-3d-vector x y z)))

(defun point->msg (point)
  (declare (type 3d-vector point))
  (make-message
   "geometry_msgs/Point"
   x (x point)
   y (y point)
   z (z point)))

(defun restamp-tf-msg (msg new-stamp)
  (with-slots (tf-msg::transforms) msg
    (loop for transform-msg across tf-msg::transforms do
      (restamp-stamped-transform-msg transform-msg new-stamp))
    msg))

(defun restamp-stamped-transform-msg (msg new-stamp)
  (with-slots ((header geometry_msgs-msg:header)) msg
    (with-slots ((stamp std_msgs-msg:stamp)) header
      (setf stamp new-stamp)
      msg)))
