(defclass mutation (unary-genetic-operator)
  ((source-selection-function :initarg :source-selection-function :accessor source-selection-function)
   (tree-creation-function :initarg :tree-creation-function :accessor tree-creation-function)
   (value-function :initarg :value-function :accessor value-function)))


(defmethod initialize-properties :after ((object mutation))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'source-selection-function :label "Source function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value 1 :editor 'function-editor)
   (:name 'tree-creation-function :label "Tree creation function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value 1 :editor 'function-editor)
   (:name 'value-function :label "Mutation function" :accessor-type 'accessor-accessor-type
    :data-type 'list :default-value 1 :editor 'list-editor
    :possible-values (mutations-value-functions))))


(defclass point-mutation (unary-genetic-operator)
  ((source-selection-function :initarg :source-selection-function :accessor source-selection-function)
   (value-function :initarg :value-function :accessor value-function)))


(defmethod initialize-properties :after ((object point-mutation))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'source-selection-function :label "Source function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value 1 :editor 'function-editor)))


(defclass lop-mutation (unary-genetic-operator)
  ((value-function :initarg :value-function :accessor value-function)))


(defmethod initialize-properties :after ((object lop-mutation))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'value-function :label "Mutation function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value 1 :editor 'function-editor)))


;;; #NOTE: Implemented for source description code extractor restrictions
(defun lambda-mutate-selection-function (x index arguments)
  (declare (ignore index))
  (node-selection-function-size-with-subexp 
   x (car (second arguments)) (first arguments) (cadr (second arguments))))

(defun lambda-point-mutation-selection-function (x index)
  (node-selection-function-terminals x index))

(defun mutations-value-functions ()
  '(mutate
    mutate-koza
    mutate-point))