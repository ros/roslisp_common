
(in-package :cl-transforms)

(defconstant +epsilon+ 0.000001)

(defun matrix->quaternion (matrix)
  "Converts a 3x3 rotation matrix to a quaternion."
  (let ((trace (+ (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2) 1.0)))
    (cond ((< trace +epsilon+)
           (let ((s (/ 0.5 (sqrt trace))))
             (make-quaternion (* (- (aref matrix 2 1) (aref matrix 1 2)) s)
                              (* (- (aref matrix 0 2) (aref matrix 2 0)) s)
                              (* (- (aref matrix 1 0) (aref matrix 0 1)) s)
                              (/ 0.25 s))))
          ((and (> (aref matrix 0 0) (aref matrix 1 1))
                (> (aref matrix 0 0) (aref matrix 2 2)))
           (let ((s (* 2.0 (sqrt (+ 1.0
                                    (aref matrix 0 0)
                                    (- (aref matrix 1 1))
                                    (- (aref matrix 2 2)))))))
             (make-quaternion (* 0.25 s)
                              (/ (+ (aref matrix 0 1) (aref matrix 1 0)) s)
                              (/ (+ (aref matrix 0 2) (aref matrix 2 0)) s)
                              (/ (- (aref matrix 2 1) (aref matrix 1 2)) s))))
          ((> (aref matrix 1 1) (aref matrix 2 2))
           (let ((s (* 2.0 (sqrt (+ 1.0
                                    (aref matrix 1 1)
                                    (- (aref matrix 0 0))
                                    (- (aref matrix 2 2)))))))
             (make-quaternion (/ (+ (aref matrix 0 1) (aref matrix 1 0)) s)
                              (* 0.25 s)
                              (/ (+ (aref matrix 1 2) (aref matrix 2 1)) s)
                              (/ (- (aref matrix 0 2) (aref matrix 2 0)) s))))
          (t
           (let ((s (* 2.0 (sqrt (+ 1.0
                                    (aref matrix 2 2)
                                    (- (aref matrix 0 0))
                                    (- (aref matrix 1 1)))))))
             (make-quaternion (/ (+ (aref matrix 0 2) (aref matrix 2 0)) s)
                              (/ (+ (aref matrix 1 2) (aref matrix 2 1)) s)
                              (* 0.25 s)
                              (/ (- (aref matrix 1 0) (aref matrix 0 1)) s)))))))

(defun matrix->transform (matrix)
  "Converts a homogenous 4x4 matrix to a pose object."
  (assert (typep matrix '(array t (4 4))))
  (let ((rotation-submatrix
         (make-array '(3 3)
                     :initial-contents (loop for y from 0 below 3
                                             collecting (loop for x from 0 below 3
                                                              collecting (aref matrix y x))))))
    (make-transform (make-3d-vector (aref matrix 0 3) (aref matrix 1 3) (aref matrix 2 3))
                    (matrix->quaternion rotation-submatrix))))

(defun transform->matrix (trans)
  "Constructs a homogenous matrix from pose and returns it as a 2D array.
   Please note: to convert it to a 1D array, you can do the following:

  (make-array (array-total-size x) 
              :element-type (array-element-type x) 
              :displaced-to x)"
  (with-slots (translation rotation) trans
    (let ((result (make-array '(4 4) :initial-element 0.0)))
      (with-slots (x y z w) rotation
        (let ((x2 (* x x))
              (y2 (* y y))
              (z2 (* z z))
              (w2 (* w w)))
          (setf (aref result 0 0) (+ w2 x2 (- y2) (- z2))
                (aref result 0 1) (- (* 2 x y) (* 2 w z))
                (aref result 0 2) (+ (* 2 x z) (* 2 w y))

                (aref result 1 0) (+ (* 2 x y) (* 2 w z))
                (aref result 1 1) (+ w2 (- x2) y2 (- z2))
                (aref result 1 2) (- (* 2 y z) (* 2 w x))

                (aref result 2 0) (- (* 2 x z) (* 2 w y))
                (aref result 2 1) (+ (* 2 y z) (* 2 w x))
                (aref result 2 2) (+ w2 (- x2) (- y2) z2))))
      (with-slots (x y z) translation
        (setf (aref result 0 3) x
              (aref result 1 3) y
              (aref result 2 3) z
              (aref result 3 3) 1))
      result)))

(defun pose->matrix (pose)
  (transform->matrix (reference-transform pose)))
