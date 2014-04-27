(defclass crossover (binary-genetic-operator)
  ((value-function :initarg :value-function :accessor value-function)))


(defmethod initialize-properties :after ((object crossover))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'value-function :label "Crossover function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'list-editor :possible-values '(crossover crossover-koza crossover-cfg))))


(defclass lop-crossover (crossover)
  ((copy-length :initarg :copy-length :accessor copy-length)))

(defmethod initialize-properties :after ((object lop-crossover))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'copy-length :label "Copy length" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value 1 :editor 'function-editor)))


