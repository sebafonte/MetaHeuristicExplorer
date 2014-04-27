
(defclass entity-function-evaluator (entity-evaluator)
  ((fitness-vector :initarg :fitness-vector :accessor fitness-vector)
   (fitness-function :initarg :fitness-function :accessor fitness-function)
   (precision :initarg :precision :accessor precision)))


(defmethod initialize-properties :after ((o entity-function-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'fitness-function :label "Fitness function" :default-value #'evaluate-distance
    :possible-values (possible-fitness-functions o) :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor :subject o)
   (:name 'precision :label "Precision" :default-value 'float :possible-values '(number double-float float single-float)
    :accessor-type 'accessor-accessor-type :data-type 'symbol :editor 'list-editor :subject o)))

(defmethod ensure-fitness-data-initialized ((o entity-function-evaluator))
  (if (not (slot-boundp o 'fitness-vector))
      (initialize-fitness-data o)))

(defmethod evaluate ((e entity-function-evaluator) (o entity-function))
  "Use <e> to calculate and answer <o> fitness."
  (funcall (fitness-function e) e o))

(defmethod evaluate ((o entity-function-evaluator) (p population)) 
  "Evaluate <p> individuals with <a>."
  (dotimes (i (population-size p))
    (evaluate o (aref (individuals-array p) i))))	

(defmethod possible-fitness-functions ((o entity-function-evaluator))
  "Answer <o> possible fitness-functions."
  '(evaluate-distance 
    evaluate-squared-distance
    evaluate-exp-error-933))