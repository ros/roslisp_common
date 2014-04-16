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

(defparameter *simple-states*
  (make-states '((:done (:send-goal :pending))
                 (:pending (:active :active
                            :preempting :active
                            :lost :done
                            :receive :done))
                 (:active (:recalling :pending
                           :lost :done
                           :receive :done)))))

(defclass simple-comm-state-machine (comm-state-machine)
  ((simple-stm :initform (make-instance 'state-machine 
                                        :current-state (getf *simple-states* :pending)
                                        :states *simple-states*)
               :accessor simple-stm))
  (:documentation "Like the comm-state-machine but it includes another state machine
                   that summarizes the other states into pending, active and done."))

(defmethod transition-to ((csm simple-comm-state-machine) signal)
  "Processes the signal and updates the state-machine and simple-state-machine.
   If the state of the simple state-machine changes the transition callback is
   called."
   (if (and (or (process-signal (stm csm) signal)
                (process-signal (simple-stm csm) signal))
            (transition-cb csm))
       (funcall (transition-cb csm))))

(defmethod comm-state ((csm simple-comm-state-machine))
  "Returns the name of the current state of the simple-comm-state-machine
   as a symbol"
  (name (get-current-state (simple-stm csm))))
  