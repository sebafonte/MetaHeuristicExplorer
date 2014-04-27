(defclass entity-language (object-with-properties)
  ())



(defclass entity-language-sample-vrp (entity-language)
  ((max-vehicles :initarg :max-interchanges :accessor max-vehicles)))

(defmethod initialize-properties :after ((o entity-language-sample-vrp))
  "Initialize <o> properties."
  (add-properties-from-values
   object
   (:name 'max-vehicles :label "Max vehicles" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 1000000 :default-value 2 :editor 'number-editor)))

(defclass entity-language-sample-lop (entity-language)
  ((max-interchanges :initarg :max-interchanges :accessor max-interchanges)))

(defmethod initialize-properties :after ((o entity-language-sample-lop))
  "Initialize <o> properties."
  (add-properties-from-values
   object
   (:name 'max-interchanges :label "Max interchanges" :accessor-type 'accessor-accessor-type 
    :data-type 'number :min-value 1 :max-value 10000000 :default-value 10000 :editor 'number-editor)))
