(defclass entity-tsp (entity)
  ())


(defmethod initialize-instance :after ((o entity) &key expresion) 
  "Initialize <o>."
  (setf (gen o) (make-instance 'genotype :expresion expresion)))

(defmethod initialize-properties-for ((o entity-tsp) (target search-task))
  "Initialize properties for <o> in target."
  (add-properties-from-values
   target
   (:name 'samples :label "Measures" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 30 :editor 'integer-editor :subject o)
   (:name 'measure-start :label "Measure start" :accessor-type 'property-accessor-type 
    :data-type 'number :default-value 0 :editor 'number-editor :subject o)
   (:name 'measure-end :label "Measure end" :accessor-type 'property-accessor-type 
    :data-type 'number :default-value 10 :editor 'number-editor :subject o)
   (:name 'fitness-vector :visible nil :accessor-type 'property-accessor-type
    :data-type 'array :default-value nil :subject o)))

(defmethod possible-fitness-functions ((o entity-tsp))
  "Answer <o> possible fitness functions."
  '(evaluate-distance))

(defmethod evaluate-distance ((a search-algorithm) (o entity-function-x-y))
  "Evaluate using squared difference with target values.
   #NOTE: fitness-vector has the target values."
  (declare (optimize (speed 3)))
  (let ((distance 0)
        (gen (copy (gen o))))
    (dotimes (i cities)
      (incf distance (distance-from 
                      (aref i gen)
                      (aref (mod (1+ i) cities) gen))))
    gen))

(defmethod drawablep ((o entity-function-x-y))
  "Answer whether o can be displayed on the GUI."
  t)
