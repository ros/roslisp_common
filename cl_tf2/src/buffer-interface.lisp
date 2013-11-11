(in-package :cl-tf2)

(defgeneric can-transform (tf target-frame source-frame &optional source-time timeout
                                                          target-time fixed-frame)
  (:documentation "Queries whether 'tf' is aware of a transform from 'source-frame' to 'target-frame'.

Optionally, one can specify a 'timeout' for the query. 'source-time' and 'target-time' specify at which time 'target-frame' and 'source-frame' should be evaluated. 'fixed-frame' denotes the frame in which the transform is assumed to be constant over time. Note: If 'target-time' is specified once also needs to specify 'fixed-frame' to avoid a run-time error."))

(defgeneric lookup-transform (tf target-frame source-frame &optional source-time timeout
                                                             target-time fixed-frame)
  (:documentation "Queries 'tf' for the transform from 'source-frame' to 'target-frame'.

Optionally, one can specify a 'timeout' for the query. 'source-time' and 'target-time' specify at which time 'target-frame' and 'source-frame' should be evaluated. 'fixed-frame' denotes the frame in which the transform is assumed to be constant over time. Note: If 'target-time' is specified once also needs to specify 'fixed-frame' to avoid a run-time error."))