
(defclass graphic (object-with-properties)
  ((name :initarg :name :initform "" :accessor name)
   (subject :initarg :subject :initform nil :accessor subject)
   (datasource-list :initarg :datasource-list :accessor datasource-list)))


;; #TODO: Delete this creator of make it lazy
(defmethod datasource ((g graphic))
  (eval (datasource-list g)))

(defmethod initialize-properties ((g graphic))
  "Initialize <g> properties."
  (add-properties-from-values
   g
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'text-editor :default-value "New draw")   
   (:name 'subject :label "Subject" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'datasource-list :label "Datasource" :accessor-type 'property-accessor-type 
    :data-type 'list :editor 'lisp-editor :default-value '(lambda (object) 0))))

(defmethod drawablep ((o graphic))
  "Answer whether <o> can be displayed on the GUI."
  t)

