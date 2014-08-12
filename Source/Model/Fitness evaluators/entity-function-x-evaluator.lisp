
(defclass entity-function-x-evaluator (entity-function-evaluator)
  ((target-program :initarg :target-program :accessor target-program)
   (samples :initarg :samples :accessor samples)
   (measure-start :initarg :measure-start :accessor measure-start)
   (measure-end :initarg :measure-end :accessor measure-end)))


;; Intent of re-initialize when requested
(defmethod fitness-vector ((o entity-function-x-evaluator))
  (mp:with-lock (*auxiliar-lock*)
    (when (null (slot-value o 'fitness-vector))
      (initialize-fitness-data o))
    (slot-value o 'fitness-vector)))

(defmethod initialize-properties :after ((o entity-function-x-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'target-program :label "Target program" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor
    :default-value '(+ 1 x (* x x) (* x x x) (* x x x x))
    :possible-values '((+ 1 x (* x x) (* x x x) (* x x x x))
                       (- (sin x) (cos x))
                       (exp (- 1 x) (* x 0.25))
                       (+ x 7 (/ x 17))))
   (:name 'samples :label "Samples" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 30 :editor 'integer-editor)
   (:name 'measure-start :label "Measure start" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 0 :editor 'number-editor)
   (:name 'measure-end :label "Measure end" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 10 :editor 'number-editor)
   (:name 'fitness-vector :visible nil :accessor-type 'accessor-accessor-type
    :data-type 'array :default-value nil)))

(defmethod initialize-fitness-data ((evaluator entity-function-x-evaluator))
  "Initialize <o> fitness data."
  (let* ((samples (samples evaluator))
         (delta (coerce (/ (- (measure-end evaluator) (measure-start evaluator)) samples) (precision evaluator)))
         (target-program (compiled-program (destination-object evaluator)))
         (x))
    (declare (special x))
    (setf (fitness-vector evaluator) (make-array (list samples 2) :element-type (precision evaluator)))
    (let ((fitness-vector (fitness-vector evaluator)))
      (dotimes (i samples)
        (setf x (+ (measure-start evaluator) (* delta i))
              (aref fitness-vector i 0) x
              (aref fitness-vector i 1) (funcall target-program))))))

(defmethod evaluate-distance ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using absolute difference with target values."
  (let ((compiler::*compiler-warnings* nil))
    (setf (fitness object) 
          (funcall (compile nil `(lambda ()
                                   (let ((error 0)
                                         (vector ,(fitness-vector evaluator)))
                                     (dotimes (i ,(samples evaluator))
                                       (let ((x (aref vector i 0)))
                                         (declare (type ,(precision evaluator) vector))
                                         (incf error (sqr (- (aref vector i 1) ,(program object))))))
                                     (/ 10 (1+ error)))))))))
  
(defmethod evaluate-squared-distance ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using squared difference with target values."
  (let ((compiler::*compiler-warnings* nil))
    (setf (fitness object) 
          (funcall (compile nil `(lambda ()
                                   (let ((error 0)
                                           (vector ,(fitness-vector evaluator)))
                                     (dotimes (i ,(samples evaluator))
                                       (let ((x (aref vector i 0)))
                                         (declare (type ,(precision evaluator) vector))
                                         (incf error (sqr (- (aref vector i 1) ,(program object))))))
                                     (/ 10 (1+ error)))))))))

(defmethod evaluate-exp-error-933 ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using absolute difference with target values."
  (let ((compiler::*compiler-warnings* nil))
    (setf (fitness object) 
          (funcall (compile nil `(lambda ()
                                   (let ((error 0)
                                         (vector ,(fitness-vector evaluator)))
                                     (dotimes (i ,(samples evaluator))
                                       (let ((x (aref vector i 0)))
                                         (declare (type ,(precision evaluator) vector))
                                         (incf error (abs (- (aref vector i 1) ,(program object))))))
                                     (* 10 (expt 0.993 error)))))))))

#|
(defmethod evaluate-distance ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using absolute difference with target values."
  (let ((function (compiled-program object)) 
        (fitness-vector (fitness-vector evaluator))
        (result) 
        (error 0)
        (x))
    (declare (special x))
    (dotimes (i (samples evaluator))
      (setf x (aref fitness-vector i 0))
      (incf error (abs (- (aref fitness-vector i 1) (funcall function)))))
    (setf (fitness object) (/ 10 (1+ error)))))

(defmethod evaluate-squared-distance ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using squared difference with target values."
  (let ((function (compiled-program object))
        (fitness-vector (fitness-vector evaluator))
        (result) 
        (x) 
        (error 0))
    (declare (special x))
    (dotimes (i (samples evaluator))
      (setf x (aref fitness-vector i 0))
      (incf error (expt (- (aref fitness-vector i 1) (funcall function)) 2)))
    (setf (fitness object) (/ 10 (1+ error)))))
|#

(defmethod reset-temporary-data :after ((evaluator entity-function-x-evaluator))
  "Clear temporary data to be used for evaluation, suck as array, flags, etc."
  (setf (fitness-vector evaluator) nil))

(defmethod objective-class ((evaluator entity-function-x-evaluator))
  'entity-function-x)

(defmethod samples-xmin ((e entity-function-x-evaluator))
  (samples-function (fitness-vector e) (samples e) 'min 0))

(defmethod samples-xmax ((e entity-function-x-evaluator))
  (samples-function (fitness-vector e) (samples e) 'max 0))

(defmethod samples-ymin ((e entity-function-x-evaluator))
  (samples-function (fitness-vector e) (samples e) 'min 1))

(defmethod samples-ymax ((e entity-function-x-evaluator))
  (samples-function (fitness-vector e) (samples e) 'max 1))

(defun samples-function (vector samples function index)
  (let ((result))
    (dotimes (i samples)
      (let ((value (aref vector i index)))
        (setf result (if result (funcall function result value) value))))
    result))
