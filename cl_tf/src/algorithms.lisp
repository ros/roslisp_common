
(in-package :cl-tf)

(defun binary-search (value array &key
                      (array-size (array-dimension array 0))
                      (lt #'<) (key #'identity))
  "Performs a binary search for `value' in `array'. This function
assumes that `array' is ordered with respect predicate `lt'.

`lt' is a function that takes two parameters and returns a non-nil
value if the second parameter is greater than the first parameter.

Returns two values, the last value that is `lt' than `value and the
first `value' that is not `lt' than `value'."
  (check-type array (array * (*)))
  (check-type lt function)
  (let ((i 0))
    (labels ((perform-search (lower upper)
               (incf i)
               (if (<= (- upper lower) 1)
                   (values (aref array lower)
                           (aref array upper))
                   (let* ((pivot-index (+ lower (truncate (/ (- upper lower) 2))))
                          (pivot (aref array pivot-index)))
                     (if (funcall lt value (funcall key pivot))
                         (perform-search lower pivot)
                         (perform-search pivot upper))))))
      (multiple-value-call #'values
        (perform-search 0 (1- array-size))
        i))))

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
