
(defclass population-generator (object-with-properties)
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)))


(defmethod initialize-properties :after ((o population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'population-generator :editor 'symbol-editor)))

(defmethod print-object ((o population-generator) seq)
  (format seq "~A" (description o)))

(defmethod generate-population ((o population-generator) (a search-algorithm))
  "Generate population for search on <algorithm>."
  (error "Subclass responsibility."))

(defmethod operate ((generator population-generator) algorithm expressions)
  "Operate on <generator> to obtain a new object for search on <algorithm>."
  (declare (ignore expressions))
  (generate-individual generator algorithm))
