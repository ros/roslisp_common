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