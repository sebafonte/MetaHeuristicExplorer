
(defclass generic-gen-evaluator ()
  ())


(defmethod initialize-properties :after ((o entity-evaluator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :default-value nil 
    :data-type 'string :editor 'text-editor)
   (:name 'solution-fitness :label "Solution fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value 9.98 :editor 'number-editor)
   (:name 'min-fitness :label "Min fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value 0 :editor 'number-editor)
   (:name 'max-fitness :label "Max fitness" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 0 :max-value 10 :default-value 10 :editor 'number-editor)))

(defmethod evaluate ((evaluator entity-evaluator) (object entity))
  "Use <evaluator> to calculate and answer <object> fitness."
  (error "Implemented by subclass."))

(defmethod objective-class ((evaluator entity-evaluator))
  'entity)

(defmethod specialize-language ((task search-task) (evaluator entity-evaluator))
  nil)
