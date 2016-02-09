;;;
;;; Copyright (c) 2010, Lorenz Moesenlechner <moesenle@in.tum.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors 
;;;       may be used to endorse or promote products derived from this software 
;;;       without specific prior written permission.
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
;;;

(in-package :cl-tf)

(defparameter *tf-broadcasting-interval* 0.1)

(defparameter *tf-static-broadcast-future-offset* 0.5)

(defun make-transform-broadcaster (&key (topic "/tf"))
  "Return a publisher that can be used with send-transform"
  (advertise topic "tf/tfMessage"))

(defun send-transform (broadcaster tr)
  "Send a stamped transform."
  (publish broadcaster (transform->tf-msg tr)))

(defun send-transforms (broadcaster &rest transforms)
  "Send stamped transforms."
  (publish broadcaster (transforms->tf-msg transforms)))

(defun send-static-transforms-blocking (broadcaster interval &rest transforms)
  (let ((msg (transforms->tf-msg transforms)))
    (loop-at-most-every interval
      (unless (eq (roslisp:node-status) :running) (return))
      (publish 
       broadcaster 
       (restamp-tf-msg msg (+ (ros-time) *tf-static-broadcast-future-offset*))))))

(defun send-static-transforms (broadcaster interval new-thread &rest transforms)
  (if new-thread
      (sb-thread:make-thread
       #'(lambda ()
           (apply #'send-static-transforms-blocking
                  broadcaster interval transforms))
       :name "TF static broadcaster thread.")
      (apply #'send-static-transforms-blocking broadcaster interval transforms)))

(defmacro with-tf-broadcasting ((broadcaster &rest transforms) &body body)
  (let ((thread-var (gensym)))
    `(let ((,thread-var (send-static-transforms ,broadcaster *tf-broadcasting-interval* t ,@transforms)))
       (unwind-protect ,@body
         (sb-thread:terminate-thread ,thread-var)))))

(defmacro with-tf-broadcasting-list ((broadcaster transforms) &body body)
  (let ((thread-var (gensym)))
    `(let ((,thread-var (apply #'send-static-transforms ,broadcaster *tf-broadcasting-interval* t ,transforms)))
       (unwind-protect ,@body
         (sb-thread:terminate-thread ,thread-var)))))
