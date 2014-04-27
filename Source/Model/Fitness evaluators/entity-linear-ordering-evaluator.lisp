(defclass entity-linear-ordering-evaluator (entity-evaluator)
  ())


(defmethod initialize-properties :after ((o entity-linear-ordering-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'solution-fitness :label "Solution fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value 1000 :editor 'number-editor)))

(defmethod evaluate ((evaluator entity-linear-ordering-evaluator) 
                     (o entity-linear-ordering))
  "Use <evaluator> to calculate and answer <o> fitness."
  (setf (fitness o) (/ (sum-value o) 1000)))

(defmethod program ((o entity-linear-ordering))
  "Answer <o> program."
  (matrix o))

(defmethod (setf program) (value (o entity-linear-ordering))
  "Set <program> to <o>."
  (setf (matrix o) value))