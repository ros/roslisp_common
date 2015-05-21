
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
