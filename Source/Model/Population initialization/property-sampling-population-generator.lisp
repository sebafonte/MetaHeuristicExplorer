
(defclass property-sampling-population-generator (population-generator)
  ((properties-sampling-description 
    :initarg :properties-sampling-description :accessor properties-sampling-description)))


(defmethod initialize-properties :after ((object property-sampling-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'properties-sampling-description :label "Properties sampling description" 
    :accessor-type 'accessor-accessor-type :data-type 'list-structure :default-value nil :editor 'lisp-editor)))

;;; #TODO:
(defmethod generate-population ((object property-sampling-population-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  nil)