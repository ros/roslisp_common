
(in-package :cl-transforms)

(defun slerp (q1 q2 ratio)
  (check-type q1 quaternion)
  (check-type q2 quaternion)
  (check-type ratio number)
  (let ((q-cos (q-dot q1 q2)))
    (let ((q-cos (if (< q-cos 0.0)
                     (- q-cos)
                     q-cos))
          (q3 (if (< q-cos 0.0)
                   (q-inv q2) q2)))
      (cond ((< (abs q-cos) (- 1 cl-transforms::*tolerance*))
             (let* ((q-sin (sqrt (- 1 (* q-cos q-cos))))
                    (angle (atan q-sin q-cos))
                    (q-inv-sin (/ q-sin))
                    (coeff-1 (* (sin (* (- 1 ratio) angle)) q-inv-sin))
                    (coeff-2 (* (sin (* ratio angle)) q-inv-sin)))
               (q+ (q-scale q1 coeff-1)
                   (q-scale q3 coeff-2))))
            (t
             (normalize (q+ (q-scale q1 (- 1 ratio))
                            (q-scale q3 ratio))))))))

(defun interpolate-vector (v-0 v-1 ratio)
  "Returns a (linearly) interpolated vector. If `ratio' is 0, `v-0' is returned,
if `ratio' is 1, `v-1' is returned."
  (check-type v-0 3d-vector)
  (check-type v-1 3d-vector)
  (check-type ratio number)
  (make-3d-vector (+ (x v-0) (* ratio (- (x v-1) (x v-0))))
                  (+ (y v-0) (* ratio (- (y v-1) (y v-0))))
                  (+ (z v-0) (* ratio (- (z v-1) (z v-0))))))
