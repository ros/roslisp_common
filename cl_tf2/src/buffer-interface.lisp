;;; Copyright (c) 2013, Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(defgeneric can-transform (tf target-frame source-frame
                           &key time timeout target-time fixed-frame)
  (:documentation "Queries whether `tf' is aware of a transform from `source-frame'
to `target-frame'.

Optionally, one can specify a `timeout' for the query. `source-time' and `target-time'
specify at which time `target-frame' and `source-frame' should be evaluated.
`fixed-frame' denotes the frame in which the transform is assumed to be constant over time.
Note: If `target-time' is specified once also needs to specify 'fixed-frame'
to avoid a run-time error."))

(defgeneric lookup-transform (tf target-frame source-frame
                              &key time timeout target-time fixed-frame)
  (:documentation "Queries `tf' for the transform from `source-frame' to `target-frame'.

Optionally, one can specify a `timeout' for the query. `source-time' and `target-time'
specify at which time `target-frame' and `source-frame' should be evaluated.
`fixed-frame' denotes the frame in which the transform is assumed to be constant over time.
Note: If 'target-time' is specified once also needs to specify `fixed-frame'
to avoid a run-time error."))

(defgeneric transform-pose (tf &key target-frame pose timeout use-current-ros-time))

(defgeneric transform-point (tf &key target-frame point timeout use-current-ros-time))
