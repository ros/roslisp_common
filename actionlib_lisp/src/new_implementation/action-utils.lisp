;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Software License Agreement (BSD License)
;; 
;; Copyright (c) 2008, Willow Garage, Inc.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with 
;; or without modification, are permitted provided that the 
;; following conditions are met:
;;
;;  * Redistributions of source code must retain the above 
;;    copyright notice, this list of conditions and the 
;;    following disclaimer.
;;  * Redistributions in binary form must reproduce the 
;;    above copyright notice, this list of conditions and 
;;    the following disclaimer in the documentation and/or 
;;    other materials provided with the distribution.
;;  * Neither the name of Willow Garage, Inc. nor the names 
;;    of its contributors may be used to endorse or promote 
;;    products derived from this software without specific 
;;    prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH 
;; DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :actionlib-lisp)

(defmacro make-action-goal-msg (client &body args)
  "Creates a message for with the action-goal-type of the `client'."
  `(make-message (action-goal-type (action-type ,client))
                 ,@args))

(defun str-has-suffix (str suffix)
  "Checks if the string `str' has `suffix' as its suffix."
  (and (> (length str) (length suffix))
       (equal (subseq str (- (length str) (length suffix)))
              suffix)))

(defun make-action-topic (a suffix)
  "Gets the name of a topic `a' and adds a slash and the `suffix'
   to the end."
  (concatenate 'string a "/" suffix))

(defun make-action-type (a suffix)
  "Creates an action-type by adding the `suffix' to the end of `a'.
   If a doesn't has 'Action' as its suffix an error is thrown."
  (assert (str-has-suffix a "Action")
          nil
          "The action type is invalid. Actions always have the suffix 'Action'")
  (concatenate 'string a suffix))

(defun action-msg-type (a suffix)
  "Creates an action-msg-type by removing 'Action' from the end of `a'
   and adding `suffix'. If a doesn't has 'Action' as its suffix an 
   error is thrown."
  (assert (str-has-suffix a "Action")
          nil
          "The action type is invalid. Actions always have the suffix 'Action'")
  (concatenate 'string
               (subseq a 0 (- (length a) (length "Action")))
               suffix))

(defun action-goal-type (a)
  "Creates an action-msg-type for the goal."
  (action-msg-type a "Goal"))

(defmacro with-timeout-handler (expires handler &body body)
  "Like sbcl's timeout macro but safe. Instead of signaling a timeout
condition, handler is executed."
  (cl-utils:with-gensyms (timer body-fun)
    `(flet ((,body-fun () ,@body))
       (if (= ,expires 0)
           (,body-fun)
           (let ((,timer (sb-ext:make-timer ,handler)))
             (sb-ext:schedule-timer ,timer ,expires)
             (unwind-protect (,body-fun)
               (sb-ext:unschedule-timer ,timer)))))))
