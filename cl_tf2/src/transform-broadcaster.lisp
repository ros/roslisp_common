;;; Copyright (c) 2015, Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(defclass transform-broadcaster ()
  ((publisher :initarg :publisher
              :reader publisher)
   (send-transform-callbacks :initform '())
   (send-transform-callbacks-enabled :initform nil)))

(defun make-transform-broadcaster (&key (topic "/tf") (static nil))
  "Returns a publisher that can be used with send-transform. The broadcasting
topic can be altered through the keyword `topic'."
  (make-instance 'transform-broadcaster
    :publisher (advertise topic "tf2_msgs/TFMessage" :latch static)))

(defgeneric send-transform (broadcaster &rest transforms)
  (:documentation "Uses `broadcaster' to send several stamped `transforms' to TF.")
  (:method ((broadcaster transform-broadcaster) &rest transforms)
    ;; (ros-info (broadcaster) "sending ~a -> ~a (at ~a)~%"
    ;;           (cl-tf-datatypes:frame-id (first transforms))
    ;;           (cl-tf-datatypes:child-frame-id (first transforms))
    ;;           (cl-tf-datatypes:stamp (car transforms)))
    (publish (slot-value broadcaster 'publisher)
             (make-message "tf2_msgs/TFMessage" :transforms (to-msg transforms)))
    (when (slot-value broadcaster 'send-transform-callbacks-enabled)
      (execute-changed-callbacks broadcaster))))

(defun execute-changed-callbacks (tf-broadcaster)
  (with-slots (send-transform-callbacks) tf-broadcaster
    (map 'nil (cl-utils:compose #'funcall #'cdr) send-transform-callbacks)))

(defun add-transforms-changed-callback (tf-broadcaster name callback)
  (with-slots (send-transform-callbacks) tf-broadcaster
    (pushnew (cons name callback) send-transform-callbacks)))

(defun remove-transforms-changed-callback (tf-broadcaster name)
  (with-slots (send-transform-callbacks) tf-broadcaster
    (setf send-transform-callbacks
          (remove name send-transform-callbacks :key #'car))))

(defun enable-changed-callbacks (tf-broadcaster)
  (setf (slot-value tf-broadcaster 'send-transform-callbacks-enabled) t))

(defun disable-changed-callbacks (tf-broadcaster)
  (setf (slot-value tf-broadcaster 'send-transform-callbacks-enabled) nil))

(defmacro with-transforms-changed-callbacks
    ((tf-broadcaster callback &key (name (gensym "TRANSFORMS-CHANGED-CALLBACK-")))
     &body body)
  "Executes `body' with enabled SEND-TRANSFORM-CALLBACKS.
`callback' is added to the overall list of the callbacks of `tf-broadcaster'."
  `(unwind-protect
        (progn
          (add-transforms-changed-callback ,tf-broadcaster ',name ,callback)
          (enable-changed-callbacks ,tf-broadcaster)
          ,@body)
     (remove-transforms-changed-callback ,tf-broadcaster ',name)
     (disable-changed-callbacks ,tf-broadcaster)))
