;;;
;;; Copyright (c) 2010, Lorenz Moesenlechner <moesenle@in.tum.de>
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
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors
;;;       may be used to endorse or promote products derived from this software
;;;       without specific prior written permission.
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
;;;

(in-package :cl-urdf)

(defun get-link-chain (urdf-tree start end)
  "Returns the chain of links from the link named `start' to the link
  named `end'"
  (labels ((walk-tree (curr end &optional path)
             (let ((from-joint (from-joint curr)))
               (cond ((eq curr end)
                      (cons curr path))
                     ((not from-joint)
                      nil)
                     ((eq (joint-type from-joint) :fixed)
                      (walk-tree (parent (from-joint curr)) end path))
                     (t
                      (walk-tree (parent (from-joint curr))
                                 end (cons curr path)))))))
    (let ((start-link (gethash start (links urdf-tree)))
          (end-link (gethash end (links urdf-tree))))
      (assert start-link nil "Link `~a' unknown" start)
      (assert end-link nil "Link `~a' unknown" end)
      (walk-tree end-link start-link))))

(defun get-joint-chain (urdf-tree start end)
  "Returns the chain of joints from the link named `start' to the link
  named `end'"
  (labels ((walk-tree (curr end &optional path)
             (let ((from-joint (from-joint curr)))
               (cond ((eq curr end)
                      path)
                     ((not from-joint)
                      nil)
                     ((eq (joint-type from-joint) :fixed)
                      (walk-tree (parent (from-joint curr)) end path))
                     (t
                      (walk-tree (parent (from-joint curr))
                                 end (cons from-joint path)))))))
    (let ((start-link (gethash start (links urdf-tree)))
          (end-link (gethash end (links urdf-tree))))
      (assert start-link nil "Link `~a' unknown" start)
      (assert end-link nil "Link `~a' unknown" end)
      (walk-tree end-link start-link))))
