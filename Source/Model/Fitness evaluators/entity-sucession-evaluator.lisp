(defclass entity-sucession-evaluator (entity-function-evaluator)
  ((value-list :initarg :value-list :accessor value-list)))


(defmethod initialize-properties :after ((o entity-sucession-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'value-list :label "Value list" :accessor-type 'accessor-accessor-type
    :data-type 'list :editor 'list-editor
    :default-value '((1 1) (2 2) (3 6) (4 42) (5 1806))
    :possible-values '(((1 1) (2 2) (3 6) (4 42) (5 1806))
                       ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6))
                       ((1 1) (2 4) (3 9) (4 16) (5 25) (6 36))))))

(defmethod evaluate-distance ((evaluator entity-sucession-evaluator) (object entity-function))
  "Evaluate using absolute difference with target values."
  (let* ((function (compiled-program object)) 
         (fitness-vector (fitness-vector evaluator))
         (value) 
         (error 0)
         (x 0))
    (declare (special x) (integer x) (number error))
    (dolist (i (value-list evaluator))
      (setf value i
            x (car value)
            value (cadr value))
      (incf error (abs (- value (funcall function)))))
    (setf (fitness object) (/ 10 (1+ error)))))

(defmethod evaluate-squared-distance ((evaluator entity-sucession-evaluator) (object entity-function))
  "Evaluate using squared difference with target values."
  (let* ((function (compiled-program object)) 
         (value) 
         (x 0) 
         (error 0))
    (declare (special x) (integer x) (number error))
    (dolist (i (value-list evaluator))
      (setf value i
            x (car value)
            value (cadr value))
      (incf error (expt (- value (funcall function)) 2)))
    (setf (fitness object) (/ 10 (1+ error)))))

(defmethod measure-start ((evaluator entity-sucession-evaluator))
  "Answer the start measure value for evaluator."
  (min (mapcar (lambda (each) (car each)) (value-list evaluator))))

(defmethod measure-end ((evaluator entity-sucession-evaluator))
  "Answer the end measure value for evaluator."
  (max (mapcar (lambda (each) (car each)) (value-list evaluator))))
