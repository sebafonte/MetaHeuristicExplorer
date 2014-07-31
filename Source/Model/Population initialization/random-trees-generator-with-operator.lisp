
(defclass random-trees-generator-with-operator (random-trees-generator)
  ((operator :initarg :operator :accessor operator)))


(defmethod initialize-properties :after ((object random-trees-generator-with-operator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'operator :label "Operator" :editor 'button-editor
    :data-type 'object :accessor-type 'accessor-accessor-type)))

(defmethod generate-program-tree ((generator random-trees-generator-with-operator) language)
  "Answer a new generated program tree on <language> using <generator>."
  (simplify language (operate (operator generator) language nil)))

(defun random-create-cfg-initial-size (program language operator)
  (declare (ignore program))
  (let ((weight-function (production-selection-weight-function operator)))
    (create-random-from-production language '(start) (max-size-new-individuals language) weight-function)))