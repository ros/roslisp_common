(in-package :cl-tf2)

(defgeneric can-transform (tf target-frame source-frame source-time timeout
                                &optional target-time fixed-frame))

(defgeneric lookup-transform (tf target-frame source-frame source-time timeout
                                   &optional target-time fixed-frame))