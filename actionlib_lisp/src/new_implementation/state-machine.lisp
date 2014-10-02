;;; Copyright (c) 2014, Jannik Buckelo <jannikbu@cs.uni-bremen.de>
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

(in-package :actionlib-lisp)

(defclass state-machine ()
  ((current-state
    :initarg :current-state
    :accessor current-state)
   (states
    :initarg :states
    :initform nil
    :accessor states)
   (stm-mutex :initform (make-mutex :name (string (gensym "state-lock")))
              :reader state-mutex)))

(defclass state ()
  ((name
    :initarg :name
    :reader name)
   (transitions
    :initarg :transitions
    :initform nil
    :accessor transitions)))

(defgeneric get-next-state (state signal)
  (:documentation "Returns the state that the transition for signal from the
                   current state points to."))

(defgeneric get-state (stm &optional state-name)
  (:documentation "Returns the state with the given name or the current state
                   if state-name is NIL."))

(defgeneric get-current-state (stm)
  (:documentation "Returns the current state of the state machine."))

(defgeneric set-current-state (stm state-name)
  (:documentation "Sets the current state of the state machine to the state with.
                   the given name. Does nothing if there is no state with that name.
                   Returns the state that the state machine was set to."))

(defgeneric process-signal (stm signal)
  (:documentation "If the current state has a transition for the signal, sets the 
                   current state to the state following the transition. Returns
                   the new state or NIL if there is no transition for the signal."))

;;; Implementation

(defmethod get-next-state ((stm state-machine) signal)
  "Returns the state that follows the `signal' transition from the 
   current state of the state-machine."
  (getf (states stm)
        (get-next-state (get-state stm) signal)))

(defmethod get-next-state ((state state) signal)
  "Returns the state that follows the `signal' transition from the `state'"
  (getf (transitions state) signal))

(defmethod get-state ((stm state-machine) &optional state-name)
  "Returns the state with name `state-name' or the current state of the state-machine
   if `state-name' is not set."
  (if state-name 
      (getf (states stm) state-name)
      (get-current-state stm)))

(defmethod process-signal ((stm state-machine) signal)
  "If there is a transition `signal' for the current state of the state-machine set
   the current state to the one following the transition. Returns the new current state
   or NIL if there is no transition for the current state."
  (let ((next-state (get-next-state stm signal)))
    (if next-state
        (set-current-state stm next-state))))

(defmethod get-current-state ((stm state-machine))
  "Returns the current state of the state-machine"
  (with-recursive-lock ((state-mutex stm))
    (current-state stm)))

(defmethod set-current-state ((stm state-machine) (state state))
  "Sets the current state to `state'."
  (with-recursive-lock ((state-mutex stm))
    (setf (current-state stm) state)))

(defmethod set-current-state ((stm state-machine) state-name)
  "Sets the current state to the state with name `state-name' if it exists."
  (let ((state (get-state stm state-name)))
    (if state
        (with-mutex ((state-mutex stm))
          (setf (current-state stm) state)))))

(defun make-states (state-transitions)
  "Gets a list with state-names followed by their transitions. The transitions
   are a list of the name of the transition followed by the target state."
  (let ((result nil))
    (loop for state-transition in state-transitions
          do (push (make-state state-transition)
                   result)
             (push (first state-transition) result))
    result))

(defun make-state (state-transition)
   "Gets a list a state-name followed by his transitions. The transitions
    are a list of the name of the transition followed by the target state."
  (make-instance 'state 
                 :name (first state-transition)
                 :transitions (second state-transition)))




                            

  
  

   
