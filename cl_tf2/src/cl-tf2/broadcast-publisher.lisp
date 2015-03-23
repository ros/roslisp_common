;;; Copyright (c) 2015 Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(defparameter *tf2-volatile-topic* "/tf")
(defparameter *tf2-static-topic* "/tf_static")
(defparameter *tf2-topic-type* "tf2_msgs/TFMessage")

(defclass broadcast-publisher ()
  ((publisher :initarg :publisher :reader publisher
              :type roslisp::publication
              :initform (advertise *tf2-volatile-topic* *tf2-topic-type*)
              :documentation "Publisher to broadcast volatile transforms.")
   (static-publisher :initarg :static-publisher :reader static-publisher
                     :type roslisp::publication
                     :initform (advertise *tf2-static-topic* *tf2-topic-type* :latch t)
                     :documentation "Publisher to broadcast static transforms."))
  (:documentation "Implementation of the tf2 'broadcast-interface'. A class which
 publishes static and volatile transforms to a remote buffer."))

(defun make-broadcast-publisher (&key (topic *tf2-volatile-topic*) (static-topic *tf2-static-topic*))
  "Creates an instance of class 'broadcast-publisher'. Keywords `topic' and `static-topic'
 allow publisher to topics other than the default ones."
  (let ((publisher (advertise topic *tf2-topic-type*))
        (static-publisher (advertise static-topic *tf2-topic-type* :latch t)))
  (make-instance 'broadcast-publisher :publisher publisher :static-publisher static-publisher)))

(defmethod send-transform ((broadcaster broadcast-publisher) &rest transforms)
  "Uses `broadcaster' to add 'transforms' as volatile transforms to a tf buffer
 connected to `broadcaster'. NOTE: Assumes that all `transforms' are of type
 'geometry_msgs/TransformStamped.'."
  (publish (publisher broadcaster) (apply #'transform->tf-msg transforms)))

(defmethod send-static-transform ((broadcaster broadcast-publisher) &rest transforms)
  "Uses `broadcaster' to add 'transforms' as static transforms to a tf buffer
 connected to `broadcaster'. NOTE: Assumes that all `transforms' are of type
 'geometry_msgs/TransformStamped.'."
  (publish (static-publisher broadcaster) (apply #'transform->tf-msg transforms)))

;;;
;;; INTERNAL
;;;

(defun transform->tf-msg (&rest transforms)
  "Returns a tf-message with `transforms' inside. NOTE: Assumes that all `transforms'
 are of type 'geometry_msgs/TransformStamped.'. NOTE: For internal use only."
  (let ((tf-transforms (mapcar #'transform-stamped->tf-transform-stamped transforms)))
    (make-msg *tf2-topic-type* :transforms (coerce  tf-transforms 'vector))))