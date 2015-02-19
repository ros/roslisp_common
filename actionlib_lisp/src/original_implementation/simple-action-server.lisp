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

(defparameter *status-publish-interval* 0.25)
(defparameter *gc-interval* 10.0)
(defvar *in-callback-thread* nil)

(defclass action-server ()
  ((pending :accessor pending :initform nil :type (or null string) :initarg :pending)
   (recalled :accessor recalled :type list :initform nil :initarg :recalled)
   (succeeded :accessor succeeded :type list :initform nil :initarg :succeeded)
   (preempted :accessor preempted :type list :initform nil :initarg :preempted)
   (aborted :accessor aborted :type list :initform nil :initarg :aborted)
   (current :accessor current :initform nil :type (or null string) :initarg :current)
   (cancelled :accessor cancelled :initform nil :type (or null string))
   (mutex :reader as-mutex :initform (make-mutex))
   (goals :reader goals :initform (make-hash-table :test #'equal))
   (deactivated-goals :reader deactivated-goals :initform (make-hash-table :test #'equal))
   (result-pub :initarg :result-pub :reader result-pub)
   (feedback-pub :initarg :feedback-pub :reader feedback-pub)
   (feedback-type :writer set-feedback-type :reader feedback-type)
   (result-type :writer set-result-type :reader result-type)
   (result-msg-type :writer set-result-msg-type :reader result-msg-type)
   (feedback-msg-type :writer set-feedback-msg-type :reader feedback-msg-type)
   (last-published-state :accessor last-published-state)
   (exec-callback :initarg :exec-callback :type function :reader exec-callback)
   (update-flag :accessor update-flag :initform t :type boolean)))

(defmethod initialize-instance :after ((as action-server) &rest args &key action snapshot)
  (declare (ignorable args))
  (let ((action (if (str-has-suffix action "Action")
                    action
                    (concatenate 'string action "Action"))))
    (unless snapshot
      (check-type action string)
      (set-result-type (action-msg-type action "Result") as)
      (set-feedback-type (action-msg-type action "Feedback") as)
      (set-result-msg-type (action-msg-type action "ActionResult") as)
      (set-feedback-msg-type (action-msg-type action "ActionFeedback") as))))

(defvar *action-server* nil
  "Dynamically bound, in action server callback threads, to the corresponding action server.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-mutating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verify-holding-mutex (as)
  (assert (eq (mutex-owner (as-mutex as)) *current-thread*)))

(defun verify-in-callback-thread ()
  (assert *in-callback-thread* nil "This operation can only be called while in a callback thread"))

(defun snapshot (as)
  "Return a snapshot of the current state (i.e., that won't change if the action server state changes) of the action server.  Not threadsafe."
  (make-instance 
   'action-server
   :pending (pending as) :recalled (recalled as)
   :succeeded (succeeded as) :preempted (preempted as)
   :aborted (aborted as) :current (current as)
   :snapshot t))

(defun make-status-message (s)
  "Create a ros message from the various lists of goals"
  (with-slots (pending recalled succeeded preempted current aborted) s
    (let ((statuses (nconc
                     (when pending (list (make-status :pending pending)))
                     (mapcar (partial #'make-status :recalled) recalled)
                     (mapcar (partial #'make-status :succeeded) succeeded)
                     (mapcar (partial #'make-status :preempted) preempted)
                     (mapcar (partial #'make-status :aborted) aborted)
                     (when current (list (make-status :active current))))))
    (make-msg "actionlib_msgs/GoalStatusArray"
              :status_list (coerce statuses 'vector)
              (:stamp :header) (ros-time) ))))


(defun get-goal-message (as id)
  "Return the stored goal message corresponding to this id"
  (let ((m (gethash id (goals as))))
    (assert m nil "Couldn't find goal with id ~a" id)
    (with-fields ((goal :goal)) m
      goal)))

(defun goals-deactivated-before (as time)
  "Assumes mutex held."
  (loop
     for g being the hash-keys of (deactivated-goals as)
     using (hash-value deactivation-time)
     when (< deactivation-time time)
     collect g))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mutating ops on action server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-goal (as g)
  "Update the state atomically given a new goal"
  (let ((m (as-mutex as))
        (id (goal-id g)))
    (with-mutex (m)
      (with-slots (pending recalled goals update-flag) as
        (setf (gethash id goals) g)
        (when pending
          (push pending recalled)
          (add-deactivated-goal pending as))
        (setq pending id
              update-flag t)
        (ros-debug (action-server state) "Added pending goal ~a" id)))))

(defun add-deactivated-goal (g as)
  (verify-holding-mutex as)
  (let ((h (deactivated-goals as)) (time (ros-time)))
    (assert (null (nth-value 1 (gethash g h))) nil 
            "Goal ~a is already deactivated" g)
    (setf (gethash g h) time)
    (ros-debug (action-server gc) "Added deactivated goal ~a at ~a" g time)
    (setf (gethash g h) time)))

(defun process-cancel-request (as)
  "Acquires mutex, removes pending goal if any, and sets the canceled goal id to equal the current goal"
  (with-slots (mutex cancelled pending current recalled) as
    (with-mutex (mutex)
      (when pending
        (push pending recalled)
        (add-deactivated-goal pending as)
        (setq pending nil))
      (when current
        (setq cancelled current)))))

    
(defun gc-old-goal (as g)
  "Garbage collect an old goal."
  (with-slots (recalled preempted aborted succeeded goals update-flag deactivated-goals) as
    (verify-holding-mutex as)
    (cond
      ((member g recalled :test #'equal) (setq recalled (remove g recalled :test #'equal)))
      ((member g preempted :test #'equal) (setq preempted (remove g preempted :test #'equal)))
        ((member g succeeded :test #'equal) (setq succeeded (remove g succeeded :test #'equal)))
        ((member g aborted :test #'equal) (setq aborted (remove g aborted :test #'equal)))
        (t (assert nil nil "Attempted to garbage collect a goal ~a that was not on the succeeded, preempted, aborted, or recalled lists.")))
      (setq update-flag t)
      (assert (remhash g goals) nil "~a was unexpectedly not in the goal list.")
      (assert (remhash g deactivated-goals) nil "~a was unexpectedly not in the deactivated goal list.")
      (ros-debug (action-server gc) "Garbage collected old goal ~a" g)))


        
(defun gc-old-goals (as time)
  (verify-holding-mutex as)
  (dolist (g (goals-deactivated-before as time))
    (gc-old-goal as g)))


(defun set-current-result (as msg status)
  "Assumes mutex is held.  Add a result message for the current goal to the result queue."
  (verify-in-callback-thread)
  (with-slots (result-pub result-msg-type current) as
    (publish result-pub (make-msg result-msg-type :status (make-status status current) :result msg))))


(defun preempt-current-fn (msg)
  "Update the state of *action-server* atomically to cause the current goal to be preempted.  Then do a dynamic nonlocal exit by throwing 'exec-callback-tag."
  (verify-in-callback-thread)
  (with-slots (current preempted update-flag mutex cancelled) *action-server*
    (with-mutex (mutex)
      (assert current nil "No active goal to preempt")
      (when cancelled
        (ros-warn (action-server state) (not (equal cancelled current))
                  "When preempting goal ~a, cancelled unexpectedly equalled ~a" current cancelled))
      (set-current-result *action-server* msg :preempted)
      (push current preempted)
      (add-deactivated-goal current *action-server*)
      (setq current nil update-flag t)
      (ros-debug (action-server state) "Preempted goal ~a" (first preempted))
      (throw 'exec-callback-tag msg))))

(defmacro preempt-current (&rest args)
  "Update the state of *action-server* atomically to cause the current goal to be preempted.  Then do a dynamic nonlocal exit by throwing 'exec-callback-tag.
The arguments are as in roslisp:make-message for the result type of the action: they alternate between (possibly nested) field specifiers and values."
  (if (= 1 (length args))
      `(preempt-current-fn ,@args)
      `(preempt-current-fn (make-msg (result-type *action-server*) ,@args))))

(defun succeed-current-fn (msg)
  "Update the state of *action-server* atomically to cause the current goal to succeed.  Then do a dynamic nonlocal exit by throwing 'exec-callback-tag."
  (verify-in-callback-thread)
  (with-slots (mutex current succeeded update-flag) *action-server*
    (with-mutex (mutex)
      (assert current nil "No active goal to succeed")
      (set-current-result *action-server* msg :succeeded)
      (push current succeeded)
      (add-deactivated-goal current *action-server*)
      (setq current nil update-flag t)
      (ros-debug (action-server state) "Goal ~a succeeded" (first succeeded))
      (throw 'exec-callback-tag msg))))

(defmacro succeed-current (&rest args)
  "Update the state of *action-server* atomically to cause the current goal to succeed.  Then do a dynamic nonlocal exit by throwing 'exec-callback-tag.
The arguments are as in roslisp:make-message for the result type of the action: they alternate between (possibly nested) field specifiers and values."
  (if (= 1 (length args))
      `(succeed-current-fn ,@args)
      `(succeed-current-fn (make-msg (result-type *action-server*) ,@args))))  

(defun abort-current-fn (msg)
  "Update the state of *action-server* atomically to cause the current goal to abort.  Then do a dynamic nonlocal exit by throwing 'exec-callback-tag."
  (verify-in-callback-thread)
  (with-slots (mutex current aborted update-flag) *action-server*
    (with-mutex (mutex)
      (assert current nil "No active goal to abort")
      (set-current-result *action-server* msg :aborted)
      (push current aborted)
      (add-deactivated-goal current *action-server*)
      (setq current nil update-flag t)
      (ros-debug (action-server state) "Goal ~a aborted" (first aborted))
      (throw 'exec-callback-tag msg))))

(defmacro abort-current (&rest args)
  "Update the state of *action-server* atomically to cause the current goal to abort.  Then do a dynamic nonlocal exit by throwing 'exec-callback-tag.
The arguments are as in roslisp:make-message for the result type of the action: they alternate between (possibly nested) field specifiers and values."
  (if (= 1 (length args))
      `(abort-current-fn ,@args)
      `(abort-current-fn (make-msg (result-type *action-server*) ,@args))))

(defun publish-feedback-fn (feedback)
  "Add feedback to the feedback queue of *action-server*"
  (with-slots (mutex feedback-pub current feedback-msg-type) *action-server*
    (with-mutex (mutex)
      (publish feedback-pub (make-msg feedback-msg-type :status (make-status :active current) :feedback feedback)))))

(defmacro publish-feedback (&rest args)
  (if (= 1 (length args))
      `(publish-feedback-fn ,(first args))
      `(publish-feedback-fn (make-msg (feedback-type *action-server*) ,@args))))

(defun cancel-request-received ()
  (verify-in-callback-thread)
  (with-slots (pending current cancelled mutex) *action-server*
    (with-mutex (mutex)
      (assert current)
      (or pending cancelled))))

(defun accept-next-goal (as)
  "Atomically accept the goal stored in pending."
  (assert (not *in-callback-thread*)) 
  (verify-holding-mutex as)
  (with-slots (current pending update-flag exec-callback cancelled) as
    (assert (and pending (null current)) nil 
            "pending was ~a and current was ~a, so can't accept-next-goal"
            pending current)
    (setq current pending
          pending nil
          update-flag t
          cancelled nil) 
    ;; We set cancelled to nil because if it's not, it must date back to a previous goal (because pending is non-null)
    (ros-debug (action-server state) "Accepted goal ~a" current)))

(defun get-current-status (as)
  "Get snapshot of the state.  Possible side effects on update flag, and the cached snapshot of the state."
  (verify-holding-mutex as)
  (with-slots (last-published-state update-flag) as
    (when update-flag
      (setq last-published-state (snapshot as)
            update-flag nil))
    (make-status-message last-published-state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROS Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun callback-loop (as)
  "Returns a loop each iteration of which calls the execution callback (which can be long-running) if a new goal is available"
  #'(lambda ()
      (let ((*action-server* as) (*in-callback-thread* t))
        (with-slots (mutex current exec-callback) as
          (let ((seen (make-hash-table :test #'equal)))
            (while (eq (node-status) :running)
              (with-mutex (mutex)
                (when current
                  (assert (null (gethash current seen)) nil 
                          "Tried to process goal ~a twice" current)
                  (setf (gethash current seen) t)))
              ;; no race condition, because nobody else can make current nil
              (when current
                (funcall exec-callback (get-goal-message as current)))
              (sleep 0.1)))
      (ros-debug (action-server callback) "Callback thread exiting")))))

(defun update-loop (as status-pub)
  "Returns a loop each iteration of which 1) publishes any messages that may have arrived on the feedback and result queues for the action server 2) publishes the status 3) accepts any pending goals 4) garbage collects old goals"
  #'(lambda ()
      (loop-at-most-every *status-publish-interval*
         (ros-debug (action-server update-loop) "Update loop")
         (unless (eq (node-status) :running) (return))
        (with-slots (mutex pending current) as
          (with-mutex (mutex)
            (gc-old-goals as (- (ros-time) *gc-interval*))
            (when (and pending (null current)) (accept-next-goal as))
            (publish status-pub (get-current-status as)))))))



(defun start-action-server (action-name action-type exec-callback &key separate-thread)
  "Start an action server.  ACTION-NAME is the base name of the associated ros topics.  ACTION-TYPE is the name of the action type (e.g., MY_PKG/MY_ACTION_TYPE).  EXEC-CALLBACK is a function of a single argument, the goal message.  See def-exec-callback for conditions on this function.  The callbacks of the action will always happen on a separate thread, but the update loop will happen on the current thread (and therefore block until the node stops running).  For nonblocking behavior, use the SEPARATE-THREAD key."
  (declare (string action-name action-type) (function exec-callback))
  (flet ((action-type (suffix)
           (cond ((str-has-suffix action-type "Action")
                  (concatenate 'string action-type suffix))
                 (t
                  (warn "Using deprecated version of action type. Please specify the type with 'Action' suffix: ~a" action-type)
                  (concatenate 'string action-type "Action" suffix))))
         (action-topic (suffix)
           (concatenate 'string action-name "/" suffix)))
    
    (let* ((action-name (fully-qualified-name action-name))
           (status-pub (advertise (action-topic "status") "actionlib_msgs/GoalStatusArray"))
           (feedback-pub (advertise (action-topic "feedback") (action-type "Feedback")))
           (result-pub (advertise (action-topic "result") (action-type "Result")))
           (as (make-instance 'action-server
                              :exec-callback exec-callback :action action-type
                              :feedback-pub feedback-pub :result-pub result-pub)))
      (make-thread (callback-loop as) 
                   :name (format nil "~a-callback-loop" action-name))
      (subscribe (action-topic "goal") (action-type "Goal") (partial #'add-goal as))
      (subscribe (action-topic "cancel") "actionlib_msgs/GoalID"
                 #'(lambda (m) (declare (ignore m)) (process-cancel-request as)))
      (if separate-thread
          (make-thread (update-loop as status-pub)
                       :name (format nil "~a-update-loop" action-name))
          (funcall (update-loop as status-pub))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros for callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-exec-callback (name args &body body)
  "A macro for defining actionlib execution callbacks.  It works like defun, except
1) The argument list is treated like the argument list of with-fields: it binds (possibly nested) fields of the goal object
2) The body is contained in a catch form called exec-callback-tag. This means you can call, e.g., preempt-current and it will automatically return from the callback.
3) If we reach the end of the body without a dynamic nonlocal exit (i.e., caused by preempting, aborting, or succeeding), an error is signalled."
  (condlet
      (((stringp (first body)) (actual-body (rest body)) (doc-string (first body)))
       (t (actual-body body) (doc-string "")))
    (let ((m (gensym)))
      `(defun ,name (,m)
         ,doc-string
         (catch 'exec-callback-tag
           (with-fields ,args ,m
             ,@actual-body
             (error "Reached the end of exec callback ~a without succeeding, aborting, or preempting" ',name)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-action-server (&rest args)
  (bind-pprint-args (str as) args
    (pprint-logical-block 
     (str nil :prefix "[" :suffix "]")
     (with-slots (pending current succeeded recalled 
                               aborted preempted) as
       (format str "Current: ~a~:@_Pending: ~a~:@_Succeeded: ~a~:@_Recalled: ~a~:@_"
               current pending succeeded recalled)
       (format str "Aborted: ~a~:@_Preempted: ~a" aborted preempted)))))

(set-pprint-dispatch 'action-server #'pprint-action-server)


(defmacro with-as-mutex (as &body body)
  (let ((m (gensym)))
    `(let ((,m (as-mutex ,as)))
       (with-mutex (,m)
         ,@body))))
