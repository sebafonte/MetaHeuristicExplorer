
(defclass graphic-property-map (graphic)
  ((property :initarg :property :accessor property)
   (value-min :initarg :value-min :accessor value-min)
   (value-max :initarg :value-max :accessor value-max)))


(defmethod initialize-properties :after ((g graphic-property-map))
  "Initialize <g> properties."
  (add-properties-from-values
   g
   (:name 'property :label "Property" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'symbol-editor)
   (:name 'datasource-list :label "Datasource" :accessor-type 'property-accessor-type 
    :data-type 'list :editor 'lisp-editor 
    :default-value '(lambda (o) (log-data-for-criteria (log-data o) :best-individual)))))

(defmethod drawablep ((o graphic-function-r-r))
  "Answer whether <o> can be displayed on the GUI."
  t)
