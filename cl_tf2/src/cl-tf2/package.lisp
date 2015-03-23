;;; Copyright (c) 2015 Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(in-package :cl-user)

(defpackage :cl-tf2
  (:use #:common-lisp #:roslisp)
  (:export 
   ;; data interface
   get-frame-id get-time-stamp apply-transform
   ;; buffer interface (simple)
   lookup-transform has-transform do-transform can-transform
   ;; buffer interface (advanced)
   lookup-transform-advanced has-transform-advanced
   do-transform-advanced can-transform-advanced
   ;; buffer client implementation
   make-buffer-client buffer-client
   ;; broadcaster interface
   send-transform send-static-transform transform-stamped->tf-transform-stamped
   ;; broadcast publisher implementation
   broadcast-publisher make-broadcast-publisher
   ;; stamped data
   header frame-id stamp make-header def-stamped
   ;; errors
   tf2-buffer-client-error tf2-lookup-error tf2-connectivity-error tf2-extrapolation-error
   tf2-invalid-argument-error tf2-timeout-error tf2-transform-error))