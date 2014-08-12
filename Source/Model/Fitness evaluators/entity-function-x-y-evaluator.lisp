(defclass entity-function-x-y-evaluator (entity-function-x-evaluator)
  ())


(defmethod initialize-properties :after ((o entity-function-x-y-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
    (:name 'samples :label "Samples" :accessor-type 'accessor-accessor-type 
     :data-type 'integer :default-value 32 :editor 'integer-editor)
    (:name 'target-program :label "Target program" :accessor-type 'accessor-accessor-type 
     :data-type 'list :editor 'lisp-editor
     :default-value '(cos (/- (+ (* x x) (* 2 y y)) 4)) 
     :possible-values '(cos (/- (+ (* x x) (* 2 y y)) 4)))))

(defmethod compiled-program ((o entity-function-x-y))
  "Answer the compiled function that representing <o> genotype."
  (compile nil `(lambda () 
                  (declare (special x) (special y)) 
                  ,(program o))))

(defmethod initialize-fitness-data ((evaluator entity-function-x-y-evaluator))
  "Fill fitness-vector on <evaluator> to prepare fitness evaluation."
  (let* ((measure-start (measure-start evaluator))
         (samples (samples evaluator))
         (delta (coerce (/ (- (measure-end evaluator) measure-start) samples) (precision evaluator)))
         (target-program (compiled-program (destination-object evaluator)))
         (x 0)
         (y 0))
    (declare (special x y)
             (type fixnum samples) 
             (type number delta x y))
    (setf (fitness-vector evaluator) (make-array (list samples samples 3) :element-type (precision evaluator)))
    (let ((fitness-vector (fitness-vector evaluator)))
      (dotimes (i samples)
        (dotimes (j samples)
          (setf x (+ measure-start (* delta i))
                y (+ measure-start (* delta j))
                (aref fitness-vector i j 0) x
                (aref fitness-vector i j 1) y
                (aref fitness-vector i j 2) (funcall target-program)))))))

(defmethod evaluate-distance ((evaluator entity-function-x-y-evaluator) (object entity-function-x-y))
  "Evaluate using absolute difference with target values."
  (let ((compiler::*compiler-warnings* nil))
    (setf (fitness object) 
          (funcall (compile nil `(lambda ()
                                   (let ((error 0)
                                         (vector ,(fitness-vector evaluator))
                                         (samples ,(samples evaluator)))
                                     (dotimes (i ,(samples evaluator))
                                       (dotimes (j ,(samples evaluator))
                                         (let ((x (aref vector i j 0))
                                               (y (aref vector i j 1)))
                                           (declare (type ,(precision evaluator) vector))
                                           (incf error (abs (- (aref vector i j 2) ,(program object)))))))
                                     (/ 10 (1+ (/ error (* samples samples)))))))))))

(defmethod evaluate-squared-distance ((evaluator entity-function-x-y-evaluator) (object entity-function-x-y))
  "Evaluate using absolute difference squared with target values."
  (let ((compiler::*compiler-warnings* nil))
    (setf (fitness object) 
          (funcall (compile nil `(lambda ()
                                   (let ((error 0)
                                         (vector ,(fitness-vector evaluator))
                                         (samples ,(samples evaluator)))
                                     (dotimes (i samples)
                                       (dotimes (j samples)
                                         (let ((x (aref vector i j 0))
                                               (y (aref vector i j 1)))
                                           (declare (type ,(precision evaluator) vector))
                                           (incf error (sqr (- (aref vector i j 2) ,(program object)))))))
                                     (/ 10 (sqr (1+ (/ error (* samples samples))))))))))))

(defmethod evaluate-exp-error-933 ((evaluator entity-function-x-y-evaluator) (object entity-function-x-y))
  "Evaluate using absolute difference with target values."
  (let ((compiler::*compiler-warnings* nil))
    (setf (fitness object) 
          (funcall (compile nil `(lambda ()
                                   (let ((error 0)
                                         (vector ,(fitness-vector evaluator))
                                         (samples ,(samples evaluator)))
                                     (dotimes (i ,(samples evaluator))
                                       (dotimes (j ,(samples evaluator))
                                         (let ((x (aref vector i j 0))
                                               (y (aref vector i j 1)))
                                           (declare (type ,(precision evaluator) vector))
                                           (incf error (abs (- (aref vector i j 2) ,(program object)))))))
                                     (* 10 (expt 0.993 error)))))))))

#|
(defmethod evaluate-distance ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using absolute difference with target values."
  (setf (fitness object) 
        (funcall (compile nil `(lambda ()
                                 (let ((error 0)
                                       (vector ,(fitness-vector evaluator)))
                                   (dotimes (i ,(samples evaluator))
                                     (let ((x (aref vector i 0)))
                                       (declare (type ,(precision evaluator) vector))
                                       (incf error (sqr (- (aref vector i 1) ,(program o))))))
                                   (/ 10 (1+ (/ error (* samples samples))))))))))

(defmethod evaluate-squared-distance ((evaluator entity-function-x-evaluator) (object entity-function-x))
  "Evaluate using squared difference with target values."
  (setf (fitness object) 
        (funcall (compile nil `(lambda ()
                                 (let ((error 0)
                                       (vector ,(fitness-vector evaluator)))
                                   (dotimes (i ,(samples evaluator))
                                     (let ((x (aref vector i 0)))
                                       (declare (type ,(precision evaluator) vector))
                                       (incf error (sqr (- (aref vector i 1) ,(program o))))))
                                   (/ 10 (sqr (1+ (/ error (* samples samples)))))))))))


(defmethod evaluate-distance ((evaluator entity-function-x-y-evaluator) (object entity-function-x-y))
  "Evaluate using absolute difference with target values."
  (let ((function (compiled-program object)) 
        (fitness-vector (fitness-vector evaluator))
        (samples (samples evaluator))
        (error 0)
        (x 0)
        (y 0))
    (declare (special x y)
             (type fixnum samples)
             (type number x y error))
    (dotimes (i samples)
      (dotimes (j samples)
        (setf x (aref fitness-vector i j 0)
              y (aref fitness-vector i j 1))
        (incf error (abs (- (aref fitness-vector i j 2) (funcall function))))))
    (setf (fitness object) 
          (/ 10 (1+ (/ error (* samples samples)))))))

(defmethod evaluate-squared-distance ((evaluator entity-function-x-y-evaluator) (object entity-function-x-y))
  "Evaluate using absolute difference squared with target values."
  (let ((function (compiled-program object))
        (samples (samples evaluator))
        (fitness-vector (fitness-vector evaluator))
        (error 0)
        (x 0)
        (y 0))
    (declare (special x y)
             (type fixnum samples)
             (type number x y error))
    (dotimes (i samples)
      (dotimes (j samples)
        (setf x (aref fitness-vector i j 0)
              y (aref fitness-vector i j 1))
        (incf error (sqr (- (aref fitness-vector i j 2) (funcall function))))))
    (setf (fitness object) 
          (/ 10 (sqr (1+ (/ error (* samples samples))))))))
|#


(defmethod objective-class ((evaluator entity-function-x-y-evaluator))
  'entity-function-x-y)
