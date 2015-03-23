;;; Copyright (c) 2015, Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(defclass header ()
  ((frame-id :initarg :frame-id :initform ""
             :reader frame-id :type string)
   (stamp :initarg :stamp :initform 0.0
          :reader stamp :type float)))

(defun make-header (frame-id stamp)
  (declare (type string frame-id)
           (type number stamp))
  (make-instance 'header :frame-id frame-id :stamp stamp))

(defmethod print-object ((obj header) strm)
  (print-unreadable-object (obj strm :type t)
    (with-slots (frame-id stamp) obj
      (format strm "(FRAME-ID: \"~a\" STAMP: ~a)" frame-id stamp))))

(defmacro def-stamped (name (slot-name slot-type &key (initform nil initform-supplied-p)))
  "Macro to define a stamped datatype and methods to get frame-id and time-stamp.

 Example usage for creating a stamped number: 
   (def-stamped number-stamped (num-value number :initform 0.0))"
  (flet ((to-keyword (sym)
           (intern (string sym) 'keyword))
         (constructor-symbol (name)
           (intern (concatenate 'string "MAKE-" (symbol-name name)))))
    `(progn
       (defclass ,name ()
         ((header :initarg :header :initform (make-instance 'cl-tf2:header)
                  :accessor header :type cl-tf2:header)
          ,(if initform-supplied-p
               `(,slot-name :initarg ,(to-keyword slot-name) :type ,slot-type
                            :accessor ,slot-name :initform ,initform)
               `(,slot-name :initarg ,(to-keyword slot-name) :type ,slot-type
                            :accessor ,slot-name))))
       (defun ,(constructor-symbol name) (,slot-name frame-id stamp)
         (make-instance 
          ',name ,(to-keyword slot-name) ,slot-name 
          :header (cl-tf2:make-header frame-id stamp)))
       (defmethod print-object ((obj ,name) strm)
         (print-unreadable-object (obj strm :type t)
           (with-slots (header ,slot-name) obj
             (format strm "~%  HEADER:~%    ~a~%  ~a:~%    ~a" 
                     header ,(string slot-name) ,slot-name))))
       (defmethod cl-tf2:get-time-stamp ((object ,name))
         (cl-tf2:stamp (cl-tf2:header object)))
       (defmethod cl-tf2:get-frame-id ((object ,name))
         (cl-tf2:frame-id (cl-tf2:header object))))))