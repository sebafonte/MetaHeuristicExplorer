
(defclass mu-lambda-evolutionary-strategy-algorithm (steady-state-algorithm)
  ((h :initarg :h :accessor h)))


(defmethod initialize-properties :after ((e mu-lambda-evolutionary-strategy-algorithm))
  "Initialize <e> properties."
  (add-properties-from-values
   e 
   (:name 'child-iterations :label "H" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :editor 'integer-editor)))

(defmethod search-loop ((a mu-lambda-evolutionary-strategy-algorithm) seed) 
  "Search method implementation for <a>." 
  nil)