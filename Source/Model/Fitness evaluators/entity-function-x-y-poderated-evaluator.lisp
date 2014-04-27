(defclass entity-function-x-y-ponderated-evaluator (entity-function-x-y-evaluator)
  ((ponderation :initarg :ponderation :accessor ponderation)))


(defmethod initialize-properties :after ((o entity-function-x-y-ponderated-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'ponderation :label "Weight" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 1 :editor 'number-editor :subject o)))

(defmethod possible-fitness-functions ((o entity-function-x-y-ponderated-evaluator))
  "Answer <o> possible fitness functions."
  '(evaluate-distance-weighted))

(defmethod evaluate-distance-weighted ((evaluator entity-function-x-y-ponderated-evaluator) 
                                       (object entity-function-x-y))
  "Evalua el gen segun la informacion previamente calculada en *fitness-vector*.
   Se suman las diferencias de los cuadrados."
  (let* ((function (compiled-program object))
         (samples (samples evaluator))
         (fitness-vector (fitness-vector evaluator))
         (result 0) 
         (error 0)
         (ponderation (ponderation evaluator))
         (x 0)
         (y 0))
    (declare (special x) (special y)
             (number x) (number y) (number samples) (number error) (number result))
    (dotimes (i samples)
      (dotimes (j samples)
        (setf result (aref fitness-vector i j)
              x (car result)
              y (cadr result))
        (incf error (sqr (- (caddr result) (funcall function))))))
    (setf (fitness object) 
          (/ 10 (sqr (1+ (/ error 
                            (* samples samples)
                            ponderation)))))))
