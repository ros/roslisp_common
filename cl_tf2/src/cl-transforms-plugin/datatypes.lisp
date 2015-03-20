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

(in-package :cl-transforms-plugin)

;;;
;;; TRANSFORM-STAMPED
;;;

(defclass transform-stamped ()
  ((header :initarg :header :reader header :type header)
   (transform :initarg :transform :reader transform :type cl-transforms:transform)
   (child-frame-id :initarg :child-frame-id :reader child-frame-id :type string)))

(defun make-transform-stamped (frame-id child-frame-id stamp transform)
  (make-instance 'transform-stamped
                 :header (make-instance 'header
                                        :frame-id frame-id
                                        :stamp stamp)
                 :child-frame-id child-frame-id
                 :transform transform))

;; (defun msg->3d-vector (msg)
  
;; (defun msg->transform (msg)
;;   (with-fields (translation rotation) msg
;;     (

;; (defun msg->transform-stamped (msg)
;;   (with-fields ((frame-id (frame_id header)) (child-frame-id child_frame_id)
;;                 transform) msg
;;     (values frame-id child-frame-id transform)))

;;;
;;; POINT-STAMPED
;;;

(def-stamped point-stamped (point cl-transforms:3d-vector :initform (cl-transforms:make-identity-vector)))


(defmethod cl-tf2:apply-transform ((object point-stamped) transform)
  (values object transform))