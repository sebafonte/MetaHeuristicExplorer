
(defclass pane-editor-entity-explorer (pane-editor-entity)
  ((children :initarg :children :initform nil :accessor children)
   (count-children :initarg :count-children :accessor count-children)
   (fail-iterations :initarg :fail-iterations :accessor fail-iterations)))


(defmethod initialize-instance :after ((p pane-editor-entity-explorer) &key key)
  "Initialize <p>."
  (declare (ignore key))
  (setf (slot-value p 'children) 
        (make-instance 'population :count-individuals (count-children p))))

(defmethod post-initialize-interface :after ((p pane-editor-entity-explorer))
  "Post initialize actions for <p>."
  (connect-interface p))

(defmethod initialize-properties :after ((p pane-editor-entity-explorer))
  "Initialize <p> properties."
  (add-properties-from-values
   p
   ;; Pane children properties
   (:name 'count-parents :label "Parents" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 8 :editor 'integer-editor)
   (:name 'count-children :label "Children" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 20 :editor 'integer-editor)
   ;; Gen edition properties
   (:name 'operation :label "Operation" :accessor-type 'property-accessor-type
    :data-type 'object :default-value nil :editor 'button-editor)
   ;; Filters
   (:name 'filters :label "Filters" :accessor-type 'property-accessor-type
    :data-type 'object :default-value nil :editor 'button-editor)
   (:name 'fail-iterations :label "Fail iterations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 10 :editor 'integer-editor)
   ;; Object inspection hook
   (:name 'selected-object :label "Model" :accessor-type 'property-accessor-type
    :data-type 'object :default-value nil :editor 'button-editor)))

(defmethod selected-operation ((p pane-editor-entity-explorer) algorithm)
  (car (find-if (lambda (each) (equal 'mutate (name (car each))))
                (operators algorithm))))

(defmethod create-node-children ((p pane-editor-entity-explorer) x y)
  (when (capi:choice-selection (pane-graph (interface p)))
    (dotimes (i (count-children p))
      (setf (aref (individuals-array (children p)) i)
            (create-node-child p x y)))
    (remap-interface-population (interface p))))

(defmethod create-node-child ((p pane-editor-entity-explorer) x y)
  "Answer a new child with program as a result of selected node operation at <x>, <y> on <p>.
   #NOTE: Simplify tree constants."
  (let* ((model (model p))
         (model-object (object model))
         (context (context model))
         (algorithm (algorithm context))
         (object (make-instance (class-of model-object)))
         (child (make-instance 'object-in-search :object object :context context))
         (child-object (object child))
         (items (capi:choice-selected-items (pane-graph (interface p))))
         (selected-node-index (1- (capi:choice-selection (pane-graph (interface p)))))
         (selected-subtree (selected-subtree model selected-node-index)))
    ;; Children creation
    (re-initialize-properties-for child-object algorithm)
    (block child-generation-block
      (dotimes (i (fail-iterations p))
        (let ((operation (copy (selected-operation p algorithm))))
          ;; Initialize operation
          (setf (source-selection-function operation)
                (lambda (subtree index arguments) 
                  (declare (ignore arguments subtree))
                  (if (= index selected-node-index) 1 0)))
          ;; Assign new program
          (setf (program child-object)
                (operate operation (language algorithm) (list (program model))))
          ;; Simplify new program
          (setf (program child-object)
                (simplify (language algorithm) (program child-object))))
        (evaluate algorithm child-object)
        (if (accepted-child p child)
            (return-from child-generation-block child))))
    child))

(defmethod accepted-child ((p pane-editor-entity-explorer) (child object-in-search))
  "Answer wheter <child> is acceptable for <p>."
  ;(let ((check-genotype (check-genotype p))
  ;      (check-phenotype (check-phenotype p)))
  ;  (not (or (includes-p p child check-genotype check-phenotype)
  ;           (constant-p child check-genotype check-phenotype))))
  t)

(defmethod set-default-editor-tab ((p pane-editor-entity-explorer))
  (declare (ignore p))
  nil)
  
(defun entity-explorer-button-action-callback (output-pane x y)
  "Callback for node second press action."
  (create-node-children (pane (capi:element-interface output-pane)) x y))

(defun entity-explorer-button-press-callback (output-pane x y)
  "Callback for node press action."
  nil)

(defun entity-explorer-button-shift-action-callback (output-pane x y)
  "Callback for node shift + second press action."
  nil)