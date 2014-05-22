
;;; Types and creators.

(deftype gl-double () 'double-float)
(deftype gl-double-vector (n) `(opengl:gl-vector :double ,n))
(deftype gl-single () 'single-float)
(deftype gl-single-vector (n) `(opengl:gl-vector :float ,n))


(defun gl-float-vector (type contents)
  (opengl:make-gl-vector type (length contents) :contents contents))

(defun gl-double-vector (&rest contents)
  (gl-float-vector :double contents))

(defun gl-single-vector (&rest contents)
  (gl-float-vector :float contents))

(defun make-gl-double-vector (size)
  (opengl:make-gl-vector :double size))

(defun make-gl-single-vector (size)
  (opengl:make-gl-vector :float size))

;;; Vertex can be pass through to 'C'
;;; vertexes list of gl-vertexes (not passed to 'C'

(declaim (inline gl-vertex gl-vertexes))
(defun gl-vertex (x y z w)
  (gl-double-vector x y z w))

(defun gl-vertexes (contents)
  (mapcar #'(lambda (c) (apply 'gl-double-vector c)) contents))


;;; XYZ coordinate
(defstruct xyz 
  (x  0.0d0 :type double-float)
  (y  0.0d0 :type double-float)
  (z  0.0d0 :type double-float))


;;; Class projection
;;; 
;;; A class which defines the fovy, aspect, near and far
;;; values for a call to glu-perspective to define the projection
;;; matrix.

(defparameter *fovy* 45.0d0)
(defparameter *aspect* 1.0d0)
(defparameter *near* 1.0d0)
(defparameter *far* 100.0d0)


(defclass projection ()
   ((fovy :initform *fovy* :initarg :fovy :accessor fovy)
    (aspect :initform *aspect* :initarg :aspect :accessor aspect)
    (near :initform *near* :initarg :near :accessor near)
    (far :initform *far* :initarg :far :accessor far)))

(defmethod draw ((projection projection))
  (opengl:glu-perspective (fovy projection) (aspect projection) (near projection) (far projection)))

(defun make-projection (&key fovy aspect near far)
  (make-instance 'projection
                 :fovy (or fovy *fovy*)
                 :aspect (or aspect *aspect*)
                 :near (or near *near*)
                 :far (or far *far*)))

;; (set-up-gl-fonts (canvas (viewer object)) object)

;;; Class camera
;;; 
;;; Defines an eye point, a center point and an up vector.
;;; The draw method calls GLU-LOOK-AT to install the camera values.

(defparameter *eye* (make-xyz :y 5.0d0))
(defparameter *center* (make-xyz))
(defparameter *up* (make-xyz :z  1.0d0))

(defclass camera ()
  ((eye :initform (copy-structure *eye*)
        :initarg :eye
        :accessor eye
        :type xyz)
   (center :initform (copy-structure *center*)
           :initarg :center
           :accessor center
           :type xyz)
   (up :initform (copy-structure *up*)
       :initarg :up
       :accessor up
       :type xyz)
   (projection :initform (make-projection)
               :initarg :projection
               :accessor projection)))


(defmethod draw ((camera camera))
  (let ((eye (eye camera))
        (center (center camera))
        (up (up camera))
        (projection (projection camera)))
    (declare (type xyz up eye center))

    (opengl:gl-matrix-mode opengl:*gl-projection*)
    (opengl:gl-load-identity)
    (draw projection)

    (opengl:gl-matrix-mode opengl:*gl-modelview*)

    (opengl:gl-load-identity)

    (opengl:glu-look-at (xyz-x eye) (xyz-y eye) (xyz-z eye)
                        (xyz-x center) (xyz-y center) (xyz-z center)
                        (xyz-x up) (xyz-y up) (xyz-z up))

    (opengl:gl-enable opengl:*gl-lighting*)

    (opengl:gl-clear-color 0.0 0.0 0.3  1.0)
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)

    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
    (opengl:gl-depth-func opengl:*gl-less*)
    (opengl:gl-enable opengl:*gl-depth-test*)))


(defun make-camera (&key eye center up projection)
  (make-instance 'camera
                 :eye (copy-structure (or eye *eye*))
                 :center (copy-structure (or center *center*))
                 :up (copy-structure (or up *up*))
                 :projection (or projection (make-projection))))

;;; Geometry Utilities

(defun vector-difference (v1 v2 res)
  (loop for i fixnum below 3 do
        (setf (opengl:gl-vector-aref res i) (- (opengl:gl-vector-aref v1 i)
                                               (opengl:gl-vector-aref v2 i))))
  res)

(defun vector-sum (v1 v2 res)
  (loop for i fixnum below 3 do
        (setf (opengl:gl-vector-aref res i) (+ (opengl:gl-vector-aref v1 i)
                                               (opengl:gl-vector-aref v2 i))))
  res)

(defun normalize (vector)
  (let* ((x (opengl:gl-vector-aref vector 0))
         (y (opengl:gl-vector-aref vector 1))
         (z (opengl:gl-vector-aref vector 2))
         (d (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (zerop d)
        (error "Can't normalize a zero-length vector! ~s" vector)
      (setf (opengl:gl-vector-aref vector 0) (/ x d)
            (opengl:gl-vector-aref vector 1) (/ y d)
            (opengl:gl-vector-aref vector 2) (/ z d)))
    vector))

(defun normalized-cross-product (v1 v2 result)
  (let ((res (or result (make-gl-double-vector (length v1)))))
    (declare (type (gl-double-vector (*)) res))
    (setf (opengl:gl-vector-aref res 0) (- (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 2))
                                           (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 1)))
          (opengl:gl-vector-aref res 1) (- (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 0))
                                           (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 2)))
          (opengl:gl-vector-aref res 2) (- (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 1))
                                           (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 0))))
    (normalize res)
    res))

(defun polar-rotate (transform dx dy)
  (declare (special *pointer-rotation-gain*))
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-rotated (float (* dx *pointer-rotation-gain*) 1.0d0) 0.0d0 0.0d0 1.0d0)
    (opengl:gl-rotated (float (* (- dy) *pointer-rotation-gain*) 1.0d0) 1.0d0 0.0d0 0.0d0)
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

