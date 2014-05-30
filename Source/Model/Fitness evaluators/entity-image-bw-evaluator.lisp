(defclass entity-image-bw-evaluator (entity-evaluator)
  ((fitness-function :initarg :fitness-function :accessor fitness-function)))


(defmethod initialize-properties :after ((o entity-image-bw-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'fitness-function :label "Fitness function" :default-value 'evaluate-squared-distance
    :possible-values (possible-fitness-functions o) :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor)))

(defmethod evaluate ((evaluator entity-image-bw-evaluator) (o entity-image-bw))
  "Use <evaluator> to calculate and answer <o> fitness."
  (funcall (fitness-function evaluator) evaluator o))

(defmethod possible-fitness-functions ((o entity-image-bw-evaluator))
  "Answer <o> possible fitness functions."
  '(evaluate-no-evaluation 
    evaluate-single-color-penalty))
