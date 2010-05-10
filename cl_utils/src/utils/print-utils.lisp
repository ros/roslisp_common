(in-package :cl-utils)

(defun force-format (str &rest args)
  "Format, followed by finish-output"
  (apply #'format str args)
  (finish-output str))