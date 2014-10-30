
(in-package :cl-transforms)

(defconstant +epsilon+ 0.000001)

(defun invert-rot-matrix (matrix)
  "Inverts a 3x3 rotation matrix."
  (transpose-rot-matrix matrix))

(defun transpose-rot-matrix (matrix)
  "Transposes a 3x3 rotation matrix."
  (assert (typep matrix '(array * (3 3))))
  (transpose-2d-matrix matrix))

(defun transpose-2d-matrix (matrix)
  "Returns the transpose of an arbitraty NxM `matrix'. 

 NOTE: Leaves `matrix' untouched, and works on a copy instead."
  (declare (type array matrix))
  (assert (= (array-rank matrix) 2))
  (destructuring-bind (rows columns) (array-dimensions matrix)
    (make-array 
     `(,columns ,rows)
     :initial-contents
     (loop for column from 0 below columns collecting
       (loop for row from 0 below rows collecting
         (aref matrix row column))))))

(defun matrix->quaternion (matrix)
  "Converts a 3x3 rotation matrix to a quaternion."
  (let ((trace (+ (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2) 1.0)))
    (cond ((> trace +epsilon+)
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
             (normalize
              (make-quaternion (/ (+ (aref matrix 0 2) (aref matrix 2 0)) s)
                               (/ (+ (aref matrix 1 2) (aref matrix 2 1)) s)
                               (* 0.25 s)
                               (/ (- (aref matrix 1 0) (aref matrix 0 1)) s))))))))

(defun quaternion->matrix (quaternion)
  "Converts a `quaternion' into a 3x3 rotation matrix."
  (let ((result (make-array '(3 3) :initial-element 0.0)))
    (with-slots (x y z w) quaternion
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
    result))

(defun column-vectors->quaternion (x y z)
  "Constructs a quaternion from the three column-vectors `x', `y',
 and `z' of a rotation matrix. Note: The column-vectors do not need
 to be normalized."
  (flet ((3d-vector->list (v)
           (with-slots (x y z) v
             (list x y z))))
    (matrix->quaternion
     (transpose-rot-matrix
      (make-array 
       '(3 3) 
       :initial-contents
       (mapcar (compose #'3d-vector->list #'normalize-vector)
               (list x y z)))))))

(defun matrix->transform (matrix)
  "Converts a homogenous 4x4 matrix to a pose object. `matrix' is a
two-dimensional 4x4 array, row-major."
  (assert (typep matrix '(array * (4 4))))
  (let ((rotation-submatrix
         (make-array '(3 3)
                     :initial-contents (loop for y from 0 below 3
                                             collecting (loop for x from 0 below 3
                                                              collecting (aref matrix y x))))))
    (make-transform (make-3d-vector (aref matrix 0 3) (aref matrix 1 3) (aref matrix 2 3))
                    (matrix->quaternion rotation-submatrix))))

(defun transform->matrix (transform)
  "Constructs a homogenous matrix from `transform' it as a 2D array."
  ;; Internal auxiliary functions.
  (flet ((set-rot-matrix (transform rotation)
           (loop for row from 0 to 2 do
             (loop for column from 0 to 2 do
               (setf (aref transform row column) 
                     (aref rotation row column)))))
         (set-trans-vector (transform translation)
           (with-slots (x y z) translation
             (setf (aref transform 0 3) x
                   (aref transform 1 3) y
                   (aref transform 2 3) z)))
         (ensure-homogeneous-transform (transform)
           (setf (aref transform 3 3) 1)))
    ;; Actual algorithm.
    (with-slots (translation rotation) transform
      (let ((result (make-array '(4 4) :initial-element 0.0)))
        (set-rot-matrix result (quaternion->matrix rotation))
        (set-trans-vector result translation)
        (ensure-homogeneous-transform result)
        result))))

(defun pose->matrix (pose)
  (transform->matrix (reference-transform pose)))
