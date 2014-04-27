
(defclass entity-function-x-values-evaluator (entity-function-evaluator)
  ((samples :initarg :samples :accessor samples)))


(defmethod initialize-properties :after ((o entity-function-x-values-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'samples :label "Samples" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor
    :default-value '((1 1) (2 2) (3 3) (4 4) (5 5)))))

(defmethod objetive-class ((evaluator entity-function-x-values-evaluator))
  'entity-function-x)

(defmethod evaluate-distance ((evaluator entity-function-x-values-evaluator) (object entity-function-x))
  "Evaluate using absolute difference with target values."
  (let ((function (compiled-program object)) 
        (result) 
        (error 0)
        (x))
    (declare (special x))
    (dolist (i (samples evaluator))
      (setf result i
            x (car result)
            result (cadr result))
      (incf error (abs (- result (funcall function)))))
    (setf (fitness object) (/ 10 (1+ error)))))

(defmethod evaluate-squared-distance ((evaluator entity-function-x-values-evaluator)
                                      (object entity-function-x))
  "Evaluate using squared difference with target values."
  (let ((function (compiled-program object)) 
        (result) 
        (error 0)
        (x))
    (declare (special x))
    (dolist (i (samples evaluator))
      (setf result i
            x (car result)
            result (cadr result))
      (incf error (expt (- result (funcall function)) 2)))
    (setf (fitness object) (/ 10 (1+ error)))))
