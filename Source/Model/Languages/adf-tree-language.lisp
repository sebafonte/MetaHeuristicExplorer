(defclass adf-tree-language (tree-language)
  ((manager :initarg :manager :accessor manager)))


(defmethod initialize-properties :after ((o adf-tree-language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'manager :label "ADFs manager" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)))