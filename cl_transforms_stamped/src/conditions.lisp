;;;
;;; Copyright (c) 2015, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
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
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
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

(define-condition transform-stamped-error (error)
  ((description :initarg :description :reader error-description))
  (:documentation "A base class for all transform stamped conditions")
  (:report (lambda (condition stream)
             (format stream (error-description condition)))))

(define-condition connectivity-error (transform-stamped-error) ()
  (:documentation "The frames requested are not connected in the reference frame tree"))

(define-condition lookup-error (transform-stamped-error) ()
  (:documentation "A frame not in the graph has been attempted to be accessed.

The most common reason for this is that the frame is not being published
or a parent frame was not set correctly causing the tree to be broken."))

(define-condition extrapolation-error (transform-stamped-error) ()
  (:documentation "The requested value would have required extrapolation beyond
current time limits."))

(define-condition invalid-argument-error (transform-stamped-error) ()
  (:documentation "One of the arguments is invalid.

Usually it is an uninitalized Quaternion (0,0,0,0)"))

(define-condition timeout-error (transform-stamped-error) ())

