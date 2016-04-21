;;; Copyright (c) 2016, Mihai Pomarlan <blandc@cs.uni-bremen.de>
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

(defun update-stamped-transform-stamp
    (stamped-transform new-stamp)
  "Return a copy of `stamped-transform' with its stamp updated to `newstamp'."
  (make-instance 'transform-stamped
                 :frame-id (frame-id stamped-transform)
                 :stamp new-stamp
                 :child-frame-id (child-frame-id stamped-transform)
                 :translation (translation stamped-transform)
                 :rotation  (rotation stamped-transform)))

;; Don't export. Convenience function used by static-transform-broadcaster
;; and derivatives to generate keys for its hashtable.
(defun get-st-key (stamped-transform)
  (list (frame-id stamped-transform) (child-frame-id stamped-transform)))

(defgeneric send-restamped-transforms (broadcaster update-stamps &rest stamped-transforms)
  (:documentation "Using `broadcaster', publish on /tf the `stamped-transforms', given as arguments.
                  The `broadcaster' might be a simple broadcaster or a static broadcaster.
                  Transforms are restamped with current ROS time iff update-stamps is true."))

(defgeneric destroy-transform-broadcaster
    (transforms-broadcaster)
  (:documentation "Unadvertise the ROS `transforms-broadcaster'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass static-transform-broadcaster ()
    ((publisher :initform (roslisp:advertise "/tf" "tf2_msgs/TFMessage")
                :reader publisher)
     ;; Hash-table elements: KEY: (frame-id child-frame-id), VALUE: transform-stamped
     (static-transforms :initform (make-hash-table :test 'equal)
                        :accessor static-transforms)))

(defmethod destroy-transform-broadcaster ((broadcaster static-transform-broadcaster))
  ;; TODO? Nothing seems to need destroying here.
  ;; NOTE: version from the Lisca branch calls roslisp:unadvertise on a wrong parameter type
  ;;       (publisher instead of topic)
  ;; NOTE: unadvertising a topic that other publishers in the CRAM ros node may use seems like
  ;;       a bad idea. Therefore, for now do nothing here.
  (declare (ignorable broadcaster)))

(defun make-static-transform-broadcaster ()
  (make-instance 'static-transform-broadcaster))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass static-transform-periodic-broadcaster (static-transform-broadcaster)
  ((frequency :accessor frequency :initform 30.0 :initarg :frequency)
   (running :initform T
            ;; Don't export this.
            :accessor running)
   (running-mutex :initform (make-instance 'sb-thread:mutex)
                  :reader running-mutex)
   (periodic-broadcaster-mutex :initform (make-instance 'sb-thread:mutex)
                               :reader periodic-broadcaster-mutex)
   (periodic-broadcaster-thread ;; to be initialized in :after, because depends on other slot values
                                :accessor periodic-broadcaster-thread)))

(defmethod initialize-instance :after ((obj static-transform-periodic-broadcaster) &key)
  (setf (periodic-broadcaster-thread obj) 
        (sb-thread:make-thread
          (lambda ()
            (let* ((start-loop-time nil)
                   (finish-loop-time nil)
                   (go-on T))
              (loop while go-on do
                (progn
                  ;; quick check to see if the thread should still run
                  (sb-thread:with-mutex ((running-mutex obj))
                    (setf go-on (running obj)))
                  (setf start-loop-time (roslisp:ros-time))
                  (send-restamped-transforms obj t)
                  (setf finish-loop-time (roslisp:ros-time))
                  (roslisp:wait-duration (- (/ 1.0 (frequency obj))
                                            (- finish-loop-time start-loop-time))))))))))

(defmethod destroy-transform-broadcaster ((broadcaster static-transform-periodic-broadcaster))
  ;; Makes the thread run out. This is achieved by politely asking, not an interrupt or terminate.
  ;; Other contents of the object (such as the hash) are not destroyed however in this implementation,
  ;; so TODO, I guess.
  (sb-thread:with-mutex ((running-mutex broadcaster))
    (setf (running broadcaster) nil)))

(defun make-static-transform-periodic-broadcaster (&optional frequency)
  (if (numberp frequency)
    (make-instance 'static-transform-periodic-broadcaster :frequency frequency)
    (make-instance 'static-transform-periodic-broadcaster)))

(defgeneric start-broadcasting-transforms (static-transform-periodic-broadcaster &rest stamped-transforms-list)
  (:documentation "Give the list of stamped transforms `stamped-transforms-list' to the
                  static-transform-periodic-broadcaster to start publishing them on TF."))

(defgeneric stop-broadcasting-transforms (static-transform-periodic-broadcaster &rest stamped-transforms-list)
  (:documentation "Give the list of stamped transforms `stamped-transforms-list' to the
                  static-transform-periodic-broadcaster to stop publishing them on TF."))


(defmethod start-broadcasting-transforms ((broadcaster static-transform-periodic-broadcaster)
                                          &rest stamped-transforms-list)
  (apply #'send-restamped-transforms broadcaster t stamped-transforms-list))

(defmethod stop-broadcasting-transforms ((broadcaster static-transform-periodic-broadcaster)
                                          &rest stamped-transforms-list)
  (sb-thread:with-mutex ((periodic-broadcaster-mutex broadcaster))
    (let* ((static-transforms-hash-table (static-transforms broadcaster)))
      ;; Remove the stamped-transforms specified in `stamped-transforms-list'
      ;; from the static transform broadcaster's hash-map.
      (mapcar
        (lambda (st)
          (remhash
            (get-st-key st)
            static-transforms-hash-table))
        stamped-transforms-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't export this. It's a convenience function that actually does the broadcast,
;; and assumes that in case of a periodic broadcaster its mutex was acquired.
(defun send-restamped-transforms-internal (broadcaster update-stamps stamped-transforms)
  (let* ((known-transforms (static-transforms broadcaster))
         (dummy (mapcar (lambda (st) ;; Add the stamped-transforms to broadcaster's hash-table.
                                     ;; NOTE: If `stamped-transform' is already in `broadcaster''s
                                     ;; hash-table then it will be simply replaced.
                          (let* ((key (get-st-key st)))
                            (setf (gethash key known-transforms)
                                  st))) 
                       stamped-transforms))
         (known-transforms (static-transforms broadcaster))
         (transforms-to-publish nil)
         (dummy2 (maphash (lambda (key transform) ;; Assemble a list of (maybe restamped)
                                                  ;; transforms to publish
                            (declare (ignore key))
                            (if update-stamps
                              (push (update-stamped-transform-stamp transform (roslisp:ros-time)) 
                                    transforms-to-publish)
                              (push transform 
                                    transforms-to-publish)))
                          known-transforms)))
    (declare (ignore dummy) (ignore dummy2))
    (roslisp:publish (publisher broadcaster)
                     (roslisp:make-message "tf2_msgs/TFMessage" 
                                           :transforms (apply #'to-msg transforms-to-publish)))))

(defmethod send-restamped-transforms ((broadcaster static-transform-broadcaster)
                                       update-stamps
                                       &rest stamped-transforms)
  "Using the simple static `broadcaster', publish on /tf the
  `stamped-transforms', given as arguments.
  If `update-stamps' then overwrite each stamp of`stamped-transforms' 
  with the current ROS time and only afterwards publish it."
  ;; A little hack to avoid writing this method twice. Check if the object defines
  ;; a periodic-broadcaster-mutex, and in that case, acquire it before broadcast.
  ;; We do the mutex acquisition here, just in case the user calls this function themselves
  ;; and doesn't leave that to the periodic broadcaster thread alone.
  (if (slot-exists-p broadcaster 'periodic-broadcaster-mutex)
    (sb-thread:with-mutex ((periodic-broadcaster-mutex broadcaster))
      (send-restamped-transforms-internal broadcaster update-stamps stamped-transforms))
    (send-restamped-transforms-internal broadcaster update-stamps stamped-transforms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following is commented out for now, pending an answer. Do we want to automatically
;; start a periodic broadcaster (if so, uncomment the lines below)? If not, then users who
;; need one will have to initialize it themselves.

;; (defparameter *static-transform-periodic-broadcaster-frequency* 30.0)
;; (defvar *static-transform-periodic-broadcaster* nil)
;; (defun initialize-stpb ()
;;   (setf *static-transform-periodic-broadcaster*
;;         (make-static-transform-broadcaster-program *static-transform-periodic-broadcaster-frequency*)))
;; (roslisp-utilities:register-ros-init-function initialize-stpb)

