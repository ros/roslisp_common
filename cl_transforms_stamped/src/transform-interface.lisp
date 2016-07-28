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

(in-package :cl-transforms-stamped)

(defgeneric lookup-transform (transformer target-frame source-frame
                              &key time timeout target-time fixed-frame)
  (:documentation "Queries `transformer' for the transform from `source-frame' to `target-frame'.

Optionally, one can specify a `timeout' for the query. `source-time' and `target-time'
specify at which time `target-frame' and `source-frame' should be evaluated.
`fixed-frame' denotes the frame in which the transform is assumed to be constant over time.
Note: If `target-time' is specified once also needs to specify `fixed-frame'
to avoid a run-time error."))

(defgeneric transform-pose-stamped (transformer
                                    &key target-frame pose timeout use-current-ros-time)
  (:documentation "Transforms `pose' from its own frame into `target-frame'."))

(defgeneric transform-point-stamped (transformer
                                     &key target-frame point timeout use-current-ros-time)
  (:documentation "Transforms `point' from its own frame into `target-frame'."))

;; note: "with multiple latched publishers within the same node only
;; one will latch correctly.".
;; roslisp runs into a single ros node!
;; this is the main reason why the roslisp node must have a unique publisher.

(defparameter *transform-broadcaster* nil
  "package variable holding the unique transform stamped broadcaster
  of a roslisp node.")

(defclass transform-broadcaster ()

  ;; note: so far there is no need to directly access these internal variables.

  (;; the unique roslisp transform plublisher.
   ;; note: the roslisp node can have
   ;; only one latched publisher working correctly
   (transform-publisher)

   ;; protects the usage of the unique roslisp transform publisher.
   (transform-publisher-mutex))

  (:documentation "simple transform stamped broadcaster.
  it publishes the given transforms stamped on /tf topic"))

(defmethod initialize-instance :after ((transform-broadcaster transform-broadcaster) &key)
  "initializes the fields of the newly created `transform-broadcaster' object."

  (setf (slot-value transform-broadcaster 'transform-publisher)
        (roslisp:advertise "/tf" "tf2_msgs/TFMessage"))

  (setf (slot-value transform-broadcaster 'transform-publisher-mutex)
        (make-instance 'sb-thread:mutex)))

(defun get-transform-broadcaster ()
  "returns the package variable `*transform-broadcaster*'.
  it represents the singleton of `*transform-broadcaster*'. "

  (or *transform-broadcaster*
      (setf *transform-broadcaster*
            (make-instance 'transform-broadcaster))))

(defun destroy-transform-broadcaster ()
  "destroys the package variable `*transform-broadcaster*' and
  unadvertises the topic /tf."

  ;; todo(lisca): consider other cases too.
  (setf *transform-broadcaster* nil)

  (roslisp:unadvertise "/tf"))

(defun broadcast-transforms (transform-broadcaster &rest transforms-stamped)
  "uses the `transform-broadcaster' in order to
  broadcast on /tf topic the `transform-stamped'"

  (sb-thread:with-mutex
      ((slot-value transform-broadcaster
                   'transform-publisher-mutex))

    ;; use the `transform-broadcaster' and
    ;; publish on /tf `transform-stamped' list of transforms stamp
    ;; note: apply threats the elements of `transform-stamped' list
    ;;       as multiple parameters rather than a single list.
    (publish
     (slot-value transform-broadcaster 'transform-publisher)
     (make-message "tf/tfMessage"
                   :transforms (apply #'vector
                                      (mapcar #'to-msg
                                              transforms-stamped))))))

