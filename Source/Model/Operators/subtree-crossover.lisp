
(defclass subtree-crossover (crossover)
  ((source-selection-function :initarg :source-selection-function :accessor source-selection-function)
   (target-selection-function :initarg :target-selection-function :accessor target-selection-function)
   (min-subtree-depth :initarg :min-subtree-depth :accessor min-subtree-depth)))


(defmethod initialize-properties :after ((o subtree-crossover))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'source-selection-function :label "Source function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'function-editor)
   (:name 'target-selection-function :label "Target function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'function-editor)
   (:name 'min-subtree-depth :label "Min. subtree depth" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'integer-editor)))

