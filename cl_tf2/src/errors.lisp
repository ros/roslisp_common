(in-package :cl-tf2)

(define-condition fixed-frame-missing-error (error)
  ((source-frame :initarg :source-frame :reader source-frame)
   (target-frame :initarg :target-frame :reader target-frame)))

(define-condition tf2-server-error (error)
  ((description :initarg :description :reader description)))

(define-condition tf2-lookup-error (tf2-server-error) ())

(define-condition tf2-connectivity-error (tf2-server-error) ())

(define-condition tf2-extrapolation-error (tf2-server-error) ())

(define-condition tf2-invalid-argument-error (tf2-server-error) ())

(define-condition tf2-timeout-error (tf2-server-error) ())

(define-condition tf2-transform-error (tf2-server-error) ())