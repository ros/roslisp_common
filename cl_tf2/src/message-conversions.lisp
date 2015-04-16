;;; Copyright (c) 2013, Georg Bartels <georg.bartels@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to
;;; endorse or promote products derived from this software without specific
;;; prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-tf2)

(defgeneric to-msg (data)
  (:documentation "Transforms 'data' into an equivalent message from geometry_msgs."))

(defgeneric from-msg (msg)
  (:documentation "Transforms 'msg' into suitable cl-tf2 or cl-transforms data structures."))

(defmethod to-msg ((data list))
  (coerce (mapcar #'to-msg data) 'vector))

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
  "cl-transforms represents 3d points as objects of class 3D-VECTOR.
They are automatically converted to geometry_msgs/Vector3.
If you need a geometry_msgs/Point use the MAKE-POINT-MSG function."
  (make-msg "geometry_msgs/Vector3"
            :x (cl-transforms:x data)
            :y (cl-transforms:y data)
            :z (cl-transforms:z data)))

(defmethod to-msg ((data cl-transforms:pose))
  (make-msg "geometry_msgs/Pose"
            :position (make-point-msg (cl-transforms:origin data))
            :orientation (to-msg (cl-transforms:orientation data))))

(defmethod to-msg ((data cl-tf-datatypes:transform-stamped))
  (make-msg "geometry_msgs/TransformStamped"
            :header (make-header-msg
                     (cl-tf-datatypes:stamp data) (cl-tf-datatypes:frame-id data))
            :child_frame_id (cl-tf-datatypes:child-frame-id data)
            :transform (to-msg (cl-transforms:make-transform
                                (cl-transforms:translation data)
                                (cl-transforms:rotation data)))))

(defmethod to-msg ((data cl-tf-datatypes:pose-stamped))
  (make-msg "geometry_msgs/PoseStamped"
            :header (make-header-msg
                     (cl-tf-datatypes:stamp data) (cl-tf-datatypes:frame-id data))
            :pose (to-msg (cl-transforms:make-pose
                           (cl-transforms:origin data)
                           (cl-transforms:orientation data)))))


(defmethod from-msg ((msg geometry_msgs-msg:TransformStamped))
  (with-fields ((frame-id (frame_id header))
                (stamp (stamp header))
                child_frame_id
                (translation-msg (translation transform))
                (rotation-msg (rotation transform)))
      msg
    (make-instance 'cl-tf-datatypes:transform-stamped
      :frame-id frame-id
      :child-frame-id child_frame_id
      :stamp stamp
      :translation (from-msg translation-msg)
      :rotation (from-msg rotation-msg))))

(defmethod from-msg ((msg geometry_msgs-msg:Transform))
  (with-fields (translation rotation) msg
    (make-instance 'cl-transforms:transform
      :translation (from-msg translation) :rotation (from-msg rotation))))

(defmethod from-msg ((msg geometry_msgs-msg:Vector3))
  (with-fields (x y z) msg
    (make-instance 'cl-transforms:3d-vector :x x :y y :z z)))

(defmethod from-msg ((msg geometry_msgs-msg:Quaternion))
  (with-fields (x y z w) msg
    (make-instance 'cl-transforms:quaternion :x x :y y :z z :w w)))

(defmethod from-msg ((msg geometry_msgs-msg:Point))
  (with-fields (x y z) msg
    (make-instance 'cl-transforms:3d-vector :x x :y y :z z)))

(defmethod from-msg ((msg geometry_msgs-msg:Pose))
  (with-fields (orientation position) msg
    (make-instance 'cl-transforms:pose
      :origin (from-msg position) :orientation (from-msg orientation))))

(defmethod from-msg ((msg geometry_msgs-msg:PoseStamped))
  (with-fields ((frame-id (frame_id header))
                (stamp (stamp header))
                (position-msg (position pose))
                (orientation-msg (orientation pose)))
      msg
    (make-instance 'cl-tf-datatypes:pose-stamped
                   :frame-id frame-id
                   :stamp stamp
                   :origin (from-msg position-msg)
                   :orientation (from-msg orientation-msg))))

(defun make-header-msg (stamp frame-id)
  (make-msg "std_msgs/Header"
            :stamp stamp
            :frame_id frame-id))

(defun make-point-msg (data)
  (declare (type cl-transforms:3d-vector data))
  (make-msg "geometry_msgs/Point"
            :x (cl-transforms:x data)
            :y (cl-transforms:y data)
            :z (cl-transforms:z data)))
