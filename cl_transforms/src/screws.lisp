;;; Copyright (c) 2014, Georg Bartels <georg.bartels@cs.uni-bremen.de>
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

(in-package :cl-transforms)

;;;
;;; TWIST REPRESENTATION
;;;

(defclass twist ()
  ((translation :initarg :translation :reader translation :type point)
   (rotation :initarg :rotation :reader rotation :type point)))

(defun make-twist (translation rotation)
  (make-instance 'twist :translation translation :rotation rotation))

(defun make-identity-twist ()
  (make-twist (make-identity-vector) (make-identity-vector)))

(defun copy-twist (twist &key translation rotation)
  (with-slots ((old-translation translation)
               (old-rotation rotation))
      twist
    (make-twist
     (or translation old-translation)
     (or rotation old-rotation))))

(defmethod print-object ((obj twist) strm)
  (print-unreadable-object (obj strm :type t)
    (with-slots (translation rotation) obj
      (format strm "~{~<~%  ~{ ~a~}~>~}" 
              `(("TRANSLATION" ,translation) ("ROTATION" ,rotation))))))

;;;
;;; WRENCH REPRESENTATION
;;;

(defclass wrench ()
  ((translation :initarg :translation :reader translation :type point)
   (rotation :initarg :rotation :reader rotation :type point)))

(defun make-wrench (translation rotation)
  (make-instance 'wrench :translation translation :rotation rotation))

(defun make-identity-wrench ()
  (make-wrench (make-identity-vector) (make-identity-vector)))

(defun copy-wrench (wrench &key translation rotation)
  (with-slots ((old-translation translation)
               (old-rotation rotation))
      wrench
    (make-wrench
     (or translation old-translation)
     (or rotation old-rotation))))

(defmethod print-object ((obj wrench) strm)
  (print-unreadable-object (obj strm :type t)
    (with-slots (translation rotation) obj
      (format strm "~{~<~%  ~{ ~a~}~>~}" 
              `(("TRANSLATION" ,translation) ("ROTATION" ,rotation))))))