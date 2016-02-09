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

(in-package :cl-tf)

(defun binary-search (value array &key
                      (start 0)
                      (end (1- (array-dimension array 0)))
                      (lt #'<) (key #'identity))
  "Performs a binary search for `value' in `array'. This function
assumes that `array' is ordered with respect to predicate `lt'.

`lt' is a function that takes two parameters and returns a non-nil
value if the second parameter is greater than the first parameter.
If `lt' is something like #'<= bad things can happen.

`key' is a function that is applied to single elements of `array'
if the element is not a trivial data structure. The result is then
compared with `lt'.

Returns two values, the last array element whose `key' value is `lt' or equal
than `value' and its subsequent array element. If `value' is in the last
element of array, the last two array elements will be returned."
  (declare (type (simple-array * 1) array))
  (check-type lt function)
  (labels ((perform-search (lower upper)
             (if (<= (- upper lower) 1)
                 (values (aref array lower)
                         (aref array upper))
                 (let* ((pivot-index (+ lower (truncate (/ (- upper lower) 2))))
                        (pivot (aref array pivot-index)))
                   (if (funcall lt value (funcall key pivot))
                       (perform-search lower pivot-index)
                       (perform-search pivot-index upper))))))
    (when (< start 0) (setf start 0))
    (when (> end (1- (array-dimension array 0))) (setf end (1- (array-dimension array 0))))
    (cond ((< end start)
           nil)
          ((funcall lt value (funcall key (aref array start)))
           nil)
          ((funcall lt (funcall key (aref array end)) value)
           nil)
          (t
           (multiple-value-call #'values
             (perform-search start end))))))

(defun lower-bound (value array &key (lt #'<) (key #'identity))
  (multiple-value-bind (lower upper)
      (binary-search value array :lt lt :key key)
    (declare (ignore upper))
    lower))

(defun upper-bound (value array &key (lt #'<) (key #'identity))
  (multiple-value-bind (lower upper)
      (binary-search value array :lt lt :key key)
    (declare (ignore lower))
    upper))
