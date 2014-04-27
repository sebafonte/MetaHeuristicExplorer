
(defclass fixed-set-constants-factory (constant-factory)
  ((constants-set :initarg :constants-set :initform nil :accessor constants-set)))


(defmethod initialize-properties :after ((o fixed-set-constants-factory))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :editor 'one-line-lisp-editor :default-value "Fixed set")
   (:name 'constants-set :label "Set" :accessor-type 'accessor-accessor-type 
    :data-type 'list-structure :editor 'one-line-lisp-editor)))

(defmethod create-constant ((o fixed-set-constants-factory))
  (random-element (constants-set o)))

(defmethod node-constant-p ((o fixed-set-constants-factory) node)
  (block nil
    (dolist (constant (constants-set o))
      (if (eq constant node) 
          (return t)))))
