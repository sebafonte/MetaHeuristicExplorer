
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


(defmethod initialize-instance :after ((o entity-image-similarity-rgb-evaluator) &rest args)
  (let ((image (gp:read-external-image (image-file o)))
        (data-length))
    (setf (image-data o) (subseq (slot-value image 'graphics-ports::data) 54)
          data-length (/ (length (image-data o)) 3)
          (image-length o) (floor data-length))
    (unless (= (ceiling data-length) (floor data-length))
      (error "Invalid bmp format."))))

(defmethod initialize-properties :after ((o entity-image-similarity-rgb-evaluator))
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
    (declare (special x) (special y) (number x) (number y) (number error))
    (dotimes (i image-x)
      (dotimes (j image-y)
        (setf y (* 10 j (/ (scale evaluator) image-y))
              x (* 10 i (/ (scale evaluator) image-x)))
        (let ((result (funcall function))
              (index (+ (* i 3) (* j 3 image-x))))
          (incf error
              (+ (abs (- (/ (aref data (+ index 0)) 256) (crop 0 1 (aref result 2))))
                 (abs (- (/ (aref data (+ index 1)) 256) (crop 0 1 (aref result 1))))
                 (abs (- (/ (aref data (+ index 2)) 256) (crop 0 1 (aref result 0))))))
          #|
           (format t "~a - i: ~a j: ~a - x: ~a y: ~a - A: ~a ~a ~a -- F: ~a ~a ~a -- E: ~a ~a ~a~%" index i j (MY-ROUND-TO-2 x) (MY-ROUND-TO-2 y)
                   (MY-ROUND-TO-2 (/ (aref data (+ index 0)) 256))
                   (MY-ROUND-TO-2 (/ (aref data (+ index 1)) 256))
                   (MY-ROUND-TO-2 (/ (aref data (+ index 2)) 256))
                   (MY-ROUND-TO-2 (crop 0 1 (aref result 2)))
                   (MY-ROUND-TO-2 (crop 0 1 (aref result 1)))
                   (MY-ROUND-TO-2 (crop 0 1 (aref result 0)))
                   (MY-ROUND-TO-2 (abs (- (/ (aref data (+ index 0)) 256) (crop 0 1 (aref result 2)))))
                   (MY-ROUND-TO-2 (abs (- (/ (aref data (+ index 1)) 256) (crop 0 1 (aref result 1)))))
                   (MY-ROUND-TO-2 (abs (- (/ (aref data (+ index 2)) 256) (crop 0 1 (aref result 0))))))
          |#
          )))
    (setf (fitness object) error)))

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
 "d:\\temp\\gg.bmp"
 '(VECTAN (VECSUBSTRACT (VECSIN (CREATEVECTOR X 0.37341067 X)) (VECCOLORMAP (CREATEVECTOR X Y 0.91291595) (CREATEVECTOR X 0.3780687 X) (VECCOLORMAP (VECABS (CREATEVECTOR X X 0.20043199)) (CREATEVECTOR 0.66052747 Y 0.4849831) (CREATEVECTOR Y 0.11076353 0.6025755))))))

(test-rgb-evaluator
 "d:\\temp\\xx.bmp"
 '(CREATEVECTOR Y Y Y))

(test-rgb-evaluator
 "d:\\temp\\ttt.bmp"
 '(VECSUBSTRACT (CREATEVECTOR X 0.010279097 0.76076985) (CREATEVECTOR Y 0.25918978 0.2027497)))

(test-rgb-evaluator
 "d:\\temp\\testing0.bmp"
 '(VECCOS (CREATEVECTOR 0.53178716 0.74702675 0.27893555)))

(test-rgb-evaluator
 "d:\\temp\\test1.bmp"
 '(VECCOLORMAP (VECSUBSTRACT (VECTAN (CREATEVECTOR Y 0.63986636 0.23364356)) (VECTAN (CREATEVECTOR X 0.84737295 Y))) (CREATEVECTOR 0.79755384 Y Y) (VECCOLORMAP (CREATEVECTOR 0.4878428 0.17382764 0.5209866) (CREATEVECTOR Y Y X) (VECSIN (CREATEVECTOR Y 0.22231208 X)))))

(test-rgb-evaluator
 "d:\\temp\\tt.bmp"
 '(VECSUBSTRACT (CREATEVECTOR X 0.010279097 0.76076985) (CREATEVECTOR Y 0.25918978 0.2027497)) )
|#