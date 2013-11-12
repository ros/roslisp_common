(in-package :cl-tf2)

(defgeneric to-msg (data)
  (:documentation "Transforms 'data' into an equivalent message from geometry_msgs."))

(defgeneric from-msg (msg)
  (:documentation "Transforms 'msg' into suitable cl-tf2 or cl-transforms data structures."))

(defmethod to-msg ((data stamped-transform))
  (make-msg "geometry_msgs/TransformStamped"
            :header (to-msg (header data))
            :child_frame_id (child-frame-id data)
            :transform (to-msg (transform data))))

(defmethod to-msg ((data header))
  (make-msg "std_msgs/Header"
            :stamp (stamp data)
            :frame_id (frame-id data)))

(defmethod to-msg ((data cl-transforms:transform))
  (make-msg "geometry_msgs/Transform"
            :translation (to-msg (cl-transforms:translation data))
            :rotation (to-msg (cl-transforms:rotation data))))

(defmethod to-msg ((data cl-transforms:quaternion))
  (make-msg "geometry_msgs/Quaternion"
            :x (cl-transforms:x data)
            :y (cl-transforms:y data)
            :z (cl-transforms:z data)
            :w (cl-transforms:w data)))

(defmethod to-msg ((data cl-transforms:3d-vector))
  (make-msg "geometry_msgs/Vector3"
            :x (cl-transforms:x data)
            :y (cl-transforms:y data)
            :z (cl-transforms:z data)))

(defmethod from-msg ((msg geometry_msgs-msg:TransformStamped))
  (with-fields (header child_frame_id transform) msg
    (make-instance 'stamped-transform 
                   :header (from-msg header)
                   :child-frame-id child_frame_id
                   :transform (from-msg transform))))

(defmethod from-msg ((msg geometry_msgs-msg:Transform))
  (with-fields (translation rotation) msg
    (make-instance 'cl-transforms:transform :translation (from-msg translation)
                                            :rotation (from-msg rotation))))

(defmethod from-msg ((msg std_msgs-msg:Header))
  (with-fields (stamp frame_id) msg
      (make-instance 'header :frame-id frame_id :stamp stamp)))

(defmethod from-msg ((msg geometry_msgs-msg:Vector3))
  (with-fields (x y z) msg
    (make-instance 'cl-transforms:3d-vector :x x :y y :z z)))

(defmethod from-msg ((msg geometry_msgs-msg:Quaternion))
  (with-fields (x y z w) msg
    (make-instance 'cl-transforms:quaternion :x x :y y :z z :w w)))