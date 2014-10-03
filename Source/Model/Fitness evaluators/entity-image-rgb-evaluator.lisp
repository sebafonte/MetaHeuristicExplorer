
(defclass entity-image-rgb-evaluator (entity-image-bw-evaluator)
  ())

(defmethod measure-start ((evaluator entity-image-rgb-evaluator))
  "Answer the start measure value for evaluator."
  0)

(defmethod measure-end ((evaluator entity-image-rgb-evaluator))
  "Answer the end measure value for evaluator."
  1)


(defclass entity-image-similarity-rgb-evaluator (entity-image-rgb-evaluator)
  ((distance-function :initarg :distance-function :accessor distance-function)
   (image-file :initarg :image-file :accessor image-file)
   (image-data :accessor image-data)
   (scale :initform 1.0 :initarg :scale :accessor scale)
   (image-length :accessor image-length)))


(defmethod initialize-instance :after ((o entity-image-rgb-evaluator) &rest args)
  (let ((image (gp:read-external-image (image-file o))))
    (setf (image-data o)
          (subseq (slot-value image 'graphics-ports::data) 54)
          (image-length o) (/ (length (image-data o)) 4))))

(defmethod initialize-properties :after ((o entity-image-rgb-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'fitness-function :label "Fitness function" :default-value 'distance-pixel-abs
    :possible-values (possible-fitness-functions o) :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor)))
                                                               
(defmethod possible-fitness-functions ((o entity-image-similarity-rgb-evaluator))
  "Answer <o> possible fitness functions."
  '(distance-pixel-abs))

(defmethod distance-pixel-abs ((evaluator entity-image-similarity-rgb-evaluator) (object entity-image-rgb))
  "Evaluate using absolute difference with target values."
  (let* ((function (compiled-program object)) 
         (data (image-data evaluator))
         (error 0)
         (x 0)
         (y 0)
         (image-x (floor (sqrt (image-length evaluator))))
         (image-y (floor image-x)))
    (declare (special x) (special y) (number x) (number y) (number error) (number result))
    (dotimes (i image-x)
      (dotimes (j image-y)
        (let ((result (funcall function))
              (index (+ x (* y (length data)))))
          (incf error
                (+ (abs (- (/ (aref data (+ index 0)) 256) (crop 0 1 (aref result 0))))
                   (abs (- (/ (aref data (+ index 1)) 256) (crop 0 1 (aref result 1))))
                   (abs (- (/ (aref data (+ index 2)) 256) (crop 0 1 (aref result 2))))))
          (incf index 4))))
    (setf (fitness object) error)
   ;(setf (/ error (* image-x image-y)) error)
    ))


(defun test-rgb-evaluator (path exp)
  (evaluate (make-instance 'entity-image-similarity-rgb-evaluator 
                           :name 'similarity-1 
                           :description "Similar to test.jpg" 
                           :image-file path
                           :scale 1.0)
            (make-instance 'entity-image-rgb
                           :expresion exp)))

#|
(test-rgb-evaluator
 "d:\\temp\\testing0.bmp"
 '(VECCOS (CREATEVECTOR 0.53178716 0.74702675 0.27893555)))

(test-rgb-evaluator
 "d:\\temp\\test1.bmp"
 '(VECCOLORMAP (VECSUBSTRACT (VECTAN (CREATEVECTOR Y 0.63986636 0.23364356)) (VECTAN (CREATEVECTOR X 0.84737295 Y))) (CREATEVECTOR 0.79755384 Y Y) (VECCOLORMAP (CREATEVECTOR 0.4878428 0.17382764 0.5209866) (CREATEVECTOR Y Y X) (VECSIN (CREATEVECTOR Y 0.22231208 X)))))

(test-rgb-evaluator
 "d:\\temp\\test2.bmp"
 '(VECCOLORMAP (VECSUBSTRACT (VECTAN (CREATEVECTOR Y 0.63986636 0.23364356)) (VECTAN (CREATEVECTOR X 0.84737295 Y))) (CREATEVECTOR 0.79755384 Y Y) (VECCOLORMAP (CREATEVECTOR 0.4878428 0.17382764 0.5209866) (CREATEVECTOR Y Y X) (VECSIN (CREATEVECTOR Y 0.22231208 X)))))

(test-rgb-evaluator
 "d:\\temp\\test3.bmp"
 '(VECCOLORMAP (VECSUBSTRACT (VECTAN (CREATEVECTOR Y 0.63986636 0.23364356)) (VECTAN (CREATEVECTOR X 0.84737295 Y))) (CREATEVECTOR 0.79755384 Y Y) (VECCOLORMAP (CREATEVECTOR 0.4878428 0.17382764 0.5209866) (CREATEVECTOR Y Y X) (VECSIN (CREATEVECTOR Y 0.22231208 X)))))

(test-rgb-evaluator
 "d:\\temp\\test4.bmp"
 '(VECCOLORMAP (VECSUBSTRACT (VECTAN (CREATEVECTOR Y 0.63986636 0.23364356)) (VECTAN (CREATEVECTOR X 0.84737295 Y))) (CREATEVECTOR 0.79755384 Y Y) (VECCOLORMAP (CREATEVECTOR 0.4878428 0.17382764 0.5209866) (CREATEVECTOR Y Y X) (VECSIN (CREATEVECTOR Y 0.22231208 X)))))
|#