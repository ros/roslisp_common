;;; Copyright (c) 2013, 2015 Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

;;;
;;; SIMPLE API
;;;

(defgeneric lookup-transform (buffer target-frame source-frame time timeout)
 (:documentation "Uses 'buffer' to query tf for a transform from 'source-frame'
 to 'target-frame' at 'time'.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available."))

(defgeneric has-transform (buffer target-frame source-frame time timeout)
 (:documentation "Predicate using 'buffer' to check whether tf is aware of a
 transform from 'source-frame' to 'target-frame' at 'time'.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available.")
  (:method (buffer target-frame source-frame time timeout)
    (handler-case (lookup-transform buffer target-frame source-frame time timeout)
      (tf2-server-error () nil))))

(defgeneric do-transform (buffer object target-frame &key timeout)
  (:documentation "Uses 'buffer' to have tf transform 'object' into 'target-frame'.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available.")
  (:method (buffer object target-frame &key (timeout 0.0))
    (apply-transform object (lookup-transform buffer target-frame (get-frame-id object)
                                              (get-time-stamp object) timeout))))

(defgeneric can-transform (buffer object target-frame &key timeout)
  (:documentation "Predicate using 'buffer' to check whether tf can transform
 'object' into reference frame 'target-frame'.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available.")
  (:method (buffer object target-frame &key timeout)
    (has-transform
     buffer target-frame (get-frame-id object) (get-time-stamp object) timeout)))

;;;
;;; ADVANCED API
;;;

(defgeneric lookup-transform-advanced (buffer target-frame target-time source-frame
                                       source-time fixed-frame timeout)
 (:documentation "Queries tf using 'buffer' for a transform from 'source-frame' to
'target-frame' 'target-frame' shall be interpreted in 'target-time' and 'source-frame'
 shall be interpreted in 'source-time'. 'fixed-frame' denotes the frame in which the
 transform is constant in time.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available."))

(defgeneric has-transform-advanced (buffer target-frame target-time source-frame
                                    source-time fixed-frame timeout)
 (:documentation "Predicate using 'buffer' to check whether tf has a transform from
'source-frame' to 'target-frame'. 'target-frame' shall be interpreted in 'target-time'
 and 'source-frame' shall be interpreted in 'source-time'. 'fixed-frame' denotes the
 frame in which the transform is constant in time.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available."))

(defgeneric do-transform-advanced (buffer object target-frame target-time fixed-frame
                                &key timeout)
 (:documentation "Uses 'buffer' to have tf transform 'object' into 'target-frame'.
 'target-frame' shall be interpreted in 'target-time. 'fixed-frame' denotes the frame
 in which the transform is constant in time.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available."))

(defgeneric can-transform-advanced (buffer object target-frame target-time fixed-frame
                                    &key timeout)
 (:documentation "Predicate using 'buffer' to check whether tf can transform 'object'
 into 'target-frame'. 'target-frame' shall be interpreted in 'target-time. 'fixed-frame'
 denotes the frame in which the transform is constant in time.

 This call will wait for the necessary transform until 'timeout' seconds have
 passed. If 'timeout' is 0, this call will wait forever until the specified
 transform is available."))
