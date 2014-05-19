
(defclass language (object-with-properties)
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (valid-new-expresion-function :initarg :valid-new-expresion-function :accessor valid-new-expresion-function)
   (operators :initarg :operators :accessor operators)))


(defmethod initialize-properties :after ((o language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type :default-value nil :data-type 'symbol 
    :editor 'text-editor)
   (:name 'description :label "Description" :accessor-type 'accessor-accessor-type :default-value "Description"
    :data-type 'string :editor 'text-editor)
   (:name 'valid-new-expresion-function :label "Valid exp function" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'list-editor :default-value 'create-new-parent-copy
    :possible-values (valid-new-expressions-functions o))
   (:name 'operators :label "Genetic operators" :accessor-type 'accessor-accessor-type 
    :data-type 'list-structure :default-value nil :editor 'object-list-probability-editor
    :setter '(setf operators))))

(defmethod (setf operators) (operators (l language))
  (setf (slot-value l 'operators) (normalize-operation-list operators)))

(defmethod languages-compatible ((a language) (b language))
  (equal (name a) (name b)))

(defmethod print-object ((o language) seq)
  (format seq "~A" (description o)))

(defmethod valid-new-expressions-functions ((o language))
  '(create-new-random-valid
    create-new-first-parent-copy
    create-new-first-parent-program-copy))

(defmethod create-new-random-valid ((l language) parents)
  (error "Subclass responsibility"))

(defmethod create-new-first-parent-program-copy ((l language) parents)
  (copy (program (first parents))))

(defmethod create-new-first-parent-copy ((l language) parents)
  (copy (first parents)))

(defmethod copy ((o language))
  (let ((copy (copy-instance o))
        (new-operators (funcall (ttrav #'cons (lambda (x) (copy x))) (operators o))))
    (setf (operators copy) new-operators)
    copy))

(defmethod update-language ((o language))
  "Update <o> internal state."
  nil)

(defmethod prepare-children-language ((l language) exp)
  exp)
