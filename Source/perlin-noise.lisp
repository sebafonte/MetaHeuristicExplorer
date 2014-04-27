(defconstant *perlin-noise-detail-level* 5)
(defconstant *seed-for-perlin-noise-pseudo-random-function* 10)


(defun linear-interpolation (a b x)
  "Answers the value for linear interpolation between a and b on point x."
  (declare (optimize (safety 0) (speed 3)))
  (+ 
   (* a (- 1 x))
   (* b x)))

(defun cosine-interpolation (a b x)
  "Answers the value for cosine interpolation between a and b on point x."
  (let ((f (* 0.5 (- 1 (cos (* x PI))))))
    (+ (* a (- 1 f))
       (* b f))))

(defun cubic-interpolation (a b c d x)
  "Answers the value for cubic interpolation between a and b on point x."
  (let* ((p (- (- d c) (- a b)))
         (q (- (- a b) p))
         (r (- c a))
         (s b))
    (+ (* p (* x x x))
       (* q (* x x))
       (* r x)
       s)))

(defmethod real-noise (x y)
  "Answers the value for a noise function on point <x>, <y>."
  (random-real 0 1))

(defmethod noise (x y)
  "Answers the value for a noise function on point <x>, <y>."
  (pseudo-random x y))

(defun smooth-noise (x y)
  "Answers the value for smooth noise on point <x>, <y>."
  (declare (optimize (safety 0) (speed 3)))
  (+ (/ (+ (noise (1- x) (1- y)) 
           (noise (1+ x) (1- y))
           (noise (1- x) (1+ y))
           (noise (1+ x) (1+ y))) 
        16)
     (/ (+ (noise (1- x) y)
           (noise (1+ x) y)
           (noise x (1- y))
           (noise x (1+ y))) 
        8)
     (/ (noise x y) 4)))

(defun interpolated-noise (x y)
  "Answer the value for an interpolated noise function on point <x>, <y>."
  (declare (optimize (safety 0) (speed 3)))
  (let ((frac-x (- x (floor x)))
        (frac-y (- y (floor y))))
    (linear-interpolation 
     (linear-interpolation (noise x y)
                           (noise (1+ x) y)
                           frac-x)
     (linear-interpolation (noise x (1+ y))
                           (noise (1+ x) (1+ y))
                           frac-x)
     frac-y)))
                          
(defun perlin-noise-x-y (x y)
  "Answer the value for the Perlin noise function on point <x>, <y>."
  (declare (optimize (safety 0) (speed 3)))
  (let ((sum 0)
        (amplitude-parameter 0.4))
    (dotimes (i *perlin-noise-detail-level*)
      (let ((frequency (sqr i))
            (amplitude (expt amplitude-parameter i)))
        (incf sum (* amplitude 
                     (interpolated-noise (* x frequency) 
                                         (* y frequency))))))
    sum))

;; #TODO: Add basic noise functions for creating images (with vec- versions)
(defun initialize-noise () 
  "Defines various default perlin noise functions."
  (let* ((number-of-points 50)
         (pseudo-random-table (make-array (list number-of-points number-of-points))))
    (dotimes (i number-of-points)
      (dotimes (j number-of-points)
        (setf (aref pseudo-random-table i j) 
              (random-real 0 1))))
    ;; Define the pseudo-random function
    (defun pseudo-random (x y)
      "Answers the value for interpolated noise on point <x>, <y>."
      (aref pseudo-random-table
            (mod (floor (mod x number-of-points)) number-of-points)
            (mod (floor (mod y number-of-points)) number-of-points)))))
