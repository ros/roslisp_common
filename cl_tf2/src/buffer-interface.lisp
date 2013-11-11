(in-package :cl-tf2)

(defgeneric can-transform (tf &key target-frame source-frame time timeout))

(defgeneric can-transform (tf &key target-frame target-time source-frame source-time
                                fixed-frame timeout))

(defgeneric lookup-transform (tf &key target-frame source-frame time timeout))

(defgeneric lookup-transform (tf &key target-frame target-time source-frame source-time
                                  fixed-frame timeout))