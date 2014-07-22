
(defclass image-vector-3d ()
  ((x :initarg :x :initform nil :accessor x)
   (y :initarg :y :initform nil :accessor y)
   (z :initarg :z :initform nil :accessor z)))


(defparameter *vector* (make-instance 'image-vector-3d))


(defmethod to-string ((object image-vector-3d))
  "Answer a string representation of <object>."
  (format nil "(~A, ~A, ~A)" (x object) (y object) (z object)))

(defmethod equals ((a image-vector-3d) (b image-vector-3d))
  "Answers whether <a> and <b> are equal."
  (and (equal (x a) (x b))
       (equal (y a) (y b))
       (equal (z a) (z b))))

(defmethod print-object ((o image-vector-3d) seq)
  (format seq "(CVEC3 ~a ~a ~a)" (x o) (y o) (z o)))

(defmethod vec-abs ((x number))
  "Answer <x> absolute value."
  (abs x))

(defmethod vec-abs ((v image-vector-3d))
  "Answer <v> absolute value."
  (sqrt (+ (sqr (vec-abs (x v)))
           (sqr (vec-abs (y v)))
           (sqr (vec-abs (z v))))))

(defmethod vec-crop ((min number) (max number) (value number))
  "Answer <value> cropped to <min>, <max>."
  (crop min max value))

(defmethod vec-crop ((min number) (max number) (value image-vector-3d))
  "Answer <value> cropped to <min>, <max>."
  (make-instance 'image-vector-3d
                 :x (crop min max (x value))
                 :y (crop min max (y value))
                 :z (crop min max (z value))))

(defmethod color-map-1 ((x number) (y number))
  "Answer RGB color map values depending on <x> and <y>."
  (setf (x *vector*) (/- x 10)
        (y *vector*) (/- y 10)
        (z *vector*) 0.1)
  (values *vector*))

(defmethod color-map-1 ((x t) (y t))
  "Answer RGB color map values depending on <x> and <y>."
  (color-map-1 (vec-abs x) 
               (vec-abs y)))

(defmethod color-map-3 ((x number) (y number) (z number))
  "Answer RGB color map values depending on <x>, <y> and <z>."
  (setf (x *vector*) (/- x 10)
        (y *vector*) (/- y 10)
        (z *vector*) (/- z 10))
  (values *vector*))

(defmethod color-map-3 ((x t) (y t) (z t))
  "Answer RGB color map values depending on <x>, <y> and <z>."
  (color-map-3 (vec-abs x)
               (vec-abs y)
               (vec-abs z)))

;; #TODO: Answer result using values for multiple-value-bind to avoid instanciation (?)
(defmacro define-unary-function-enclosure (name operation type)
  `(progn 
     (defmethod ,name ((a ,type))
       (make-instance 'image-vector-3d 
                      :x (,operation (x a))
                      :y (,operation (y a))
                      :z (,operation (z a))))
     (defmethod ,name ((a number))
       (,operation a))))

;; #NOTE: Maybe not necesary in this way
(defmacro define-binary-function-enclosure (name operation type)
  `(progn 
     (defmethod ,name ((a ,type) (b ,type))
       (make-instance 'image-vector-3d 
                      :x (,operation (vec-abs a) (vec-abs b))
                      :y (,operation (vec-abs a) (vec-abs b))
                      :z (,operation (vec-abs a) (vec-abs b))))
     (defmethod ,name ((a number) (b number))
       (values (,operation a a)))
     (defmethod ,name ((a ,type) (b number))
       (make-instance 'image-vector-3d 
                      :x (,operation (vec-abs a) b)
                      :y (,operation (vec-abs a) b)
                      :z (,operation (vec-abs a) b)))
     (defmethod ,name ((a number) (b ,type))
       (make-instance 'image-vector-3d 
                      :x (,operation a (vec-abs b))
                      :y (,operation a (vec-abs b))
                      :z (,operation a (vec-abs b))))))

(defun define-enclosure-functions-image-vector ()
  "Define special vector functions for some image related DSL."
  ;; Unary
  (define-unary-function-enclosure vec-sin sin image-vector-3d)
  (define-unary-function-enclosure vec-cos cos image-vector-3d)
  (define-unary-function-enclosure vec-tan tan image-vector-3d)
  (define-unary-function-enclosure vec-log log image-vector-3d)
  (define-unary-function-enclosure vec-exp exp image-vector-3d)
  ;; Binary
  (define-binary-function-enclosure vec-+ + image-vector-3d)
  (define-binary-function-enclosure vec-- - image-vector-3d)
  (define-binary-function-enclosure vec-* * image-vector-3d)
  (define-binary-function-enclosure vec-/- /- image-vector-3d)
  (define-binary-function-enclosure vec-++ + image-vector-3d)
  (define-binary-function-enclosure vec--- - image-vector-3d)
  (define-binary-function-enclosure vec-** * image-vector-3d)
  (define-binary-function-enclosure vec-/-- /- image-vector-3d)
  ;; Noise functions
  (define-binary-function-enclosure vec-perlin-x-y perlin-noise-x-y image-vector-3d)
  (define-binary-function-enclosure vec-inoise-x-y interpolated-noise image-vector-3d))

(defun initialize-image-vector-functions ()
  "Initialize special vector functions for some image related DSL."
  (define-enclosure-functions-image-vector))

