
(defclass entity-evaluator (object-with-properties)
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (solution-fitness :initarg :solution-fitness :accessor solution-fitness)
   (min-fitness :initarg :min-fitness :accessor min-fitness)
   (max-fitness :initarg :max-fitness :accessor max-fitness)
   (evaluations :initform 0 :accessor evaluations)))


(defmethod initialize-properties :after ((o entity-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :default-value nil 
    :data-type 'string :editor 'text-editor)
   (:name 'solution-fitness :label "Solution fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value nil :editor 'number-editor)
   (:name 'min-fitness :label "Min fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value 0 :editor 'number-editor)
   (:name 'max-fitness :label "Max fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value 10 :editor 'number-editor)
   (:name 'evaluations :label "Evaluations" :read-only t
    :accessor-type 'accessor-accessor-type :data-type 'integer :editor 'integer-editor)))

(defmethod print-object ((o entity-evaluator) seq)
  (format seq "~A" (description o)))

(defmethod initialize-fitness-data ((o entity-evaluator))
  "Initialize <o> fitness data."
  nil)

(defmethod ensure-fitness-data-initialized ((o entity-evaluator))
  nil)

(defmethod destination-object ((o entity-evaluator))
  (make-instance (objective-class o) :expresion (target-program o)))

(defmethod reset-temporary-data ((o entity-evaluator))
  "Clear temporary data used on <evaluator>."
  nil)

(defmethod objective-class ((o entity-evaluator))
  'entity)

(defmethod specialize-language ((task search-task) (e entity-evaluator))
  nil)
  
(defmethod evaluate-no-evaluation ((e entity-evaluator) (o entity-image-bw))
  "Answer a constant value to avoid evaluation."
  (setf (fitness o) 1))

(defmethod evaluate ((o entity-evaluator) (p population)) 
  "Evaluate <p> individuals with <a>."
  (dotimes (i (size p))
    (evaluate o (aref (individuals-array p) i))))

(defmethod evaluate :after ((o entity-evaluator) (e entity)) 
  "Evaluate <p> individuals with <a>."
  (incf (evaluations o)))