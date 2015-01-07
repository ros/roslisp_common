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


(in-package :actionlib)

;;; This file contains (non-mutating) utils that are used by both, the
;;; server and the client implementations.

(defgeneric goal-id (g))
(defgeneric get-status-symbol (s))

(defmethod goal-id ((g ros-message))
  "Takes in a FooActionGoal message and returns the id string"
  (with-fields ((id (:id :goal_id))) g
    id))

(defmethod get-status-symbol ((status <GoalStatus>))
  (let ((pair (rassoc (status status)
                      (symbol-codes '<GoalStatus>))))
    (assert pair () "Could not find status code for `~a'" status)
    (car pair)))

(defun action-package (msg-type)
  (etypecase msg-type
    (symbol (symbol-package msg-type))
    (string
       (destructuring-bind (pkg-name type) (roslisp-utils:tokens
                                            (string-upcase msg-type)
                                            :separators '(#\/))
         (declare (ignore type))
         (find-package (intern (concatenate 'string pkg-name "-MSG") 'keyword))))))

(defun make-status (status goal)
  (make-msg "actionlib_msgs/GoalStatus"
            (id goal_id) goal
            status (symbol-code '<GoalStatus> status)))

(defun str-has-suffix (str suffix)
  (and (> (length str) (length suffix))
       (equal (subseq str (- (length str) (length suffix)))
              suffix)))

(defun action-topic (a suffix)
  (concatenate 'string a "/" suffix))

(defun action-type (a suffix)
  (assert (str-has-suffix a "Action")
          nil
          "The action type is invalid. Actions always have the suffix 'Action'")
  (concatenate 'string a suffix))

(defun action-msg-type (a suffix)
  (assert (str-has-suffix a "Action")
          nil
          "The action type is invalid. Actions always have the suffix 'Action'")
  (concatenate 'string
               (subseq a 0 (- (length a) (length "Action")))
               suffix))

(defun action-goal-type (a)
  (action-msg-type a "Goal"))

;; Needed to prevent problems with older and newer sbcl versions
(defun lisp-version-number ()
  (parse-integer (remove #\. (lisp-implementation-version)) :junk-allowed t))
