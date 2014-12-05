;;; Copyright (c) 2014, Jan Winkler <winkler@cs.uni-bremen.de>
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

(defun unslash-frame (frame)
  "Removes any leading or trailing '/' characters from the string
`frame' and returns the resulting string."
  (string-trim "/" frame))

(defun ensure-pose-stamped-transformed (tf pose-stamped target-frame
                                        &key use-current-ros-time)
  "Tries to transform the stamped pose `pose-stamped' into the TF
frame `target-frame'. When transformation fails, it is retried until
it succeeds. If the option `use-current-ros-time' is set to `t', the
time stamp in `pose-stamped' is replaced by the most current ROS time
via `(roslisp:ros-time)' before trying to transform it every time. The
parameter `tf' must be a valid instance of type
`cl-tf2:buffer-client'. Returns the transformed pose."
  (let ((target-frame (unslash-frame target-frame))
        (source-frame (unslash-frame (tf:frame-id pose-stamped))))
    (loop with result = nil
          while (not result)
          as rostime = (cond (use-current-ros-time (roslisp:ros-time))
                             (t (tf:stamp pose-stamped)))
          as transform = (cl-tf2:can-transform
                          tf
                          target-frame
                          source-frame
                          rostime 3.0)
          when transform
            do (let* ((cl-transforms-transform
                        (cl-tf2::transform transform))
                      (transformed-pose
                        (cl-transforms:transform-pose
                         cl-transforms-transform
                         pose-stamped)))
                 (setf result (tf:make-pose-stamped
                               target-frame
                               rostime
                               (cl-transforms:origin transformed-pose)
                               (cl-transforms:orientation transformed-pose))))
          finally (return result))))

(defun ensure-transform-available (tf reference-frame target-frame)
  "Tries to find a valid transformation between the frames
`reference-frame' and `target-frame' until one is available. The
parameter `tf' must be a valid instance of type
`cl-tf2:buffer-client'. Returns the found transformation."
  (let ((target-frame (unslash-frame target-frame))
        (reference-frame (unslash-frame reference-frame))
        (first-run t))
    (loop for sleepiness = (or first-run (sleep 1.0))
          for time = (ros-time)
          for can-tr = (let ((can-tr (cl-tf2:can-transform
                                      tf
                                      target-frame
                                      reference-frame
                                      time 2.0)))
                         (when can-tr
                           (tf:transform->stamped-transform
                            reference-frame
                            target-frame
                            time
                            (cl-tf2::transform can-tr))))
          when (progn
                 (setf first-run nil)
                 can-tr)
            do (return can-tr))))
