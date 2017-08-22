;;; 
;;; Entity editor pane
;;;
;;; Possible interface mode, 'pane-editor-entity as default:
;;;  'interface-pane-editor-entity
;;;  'interface-pane-editor-entity-properties
;;;  'interface-pane-editor-entity-image
;;;
;;; #TODO: This should be the standard editor
;;; #NOTE: Temporary implementation detail, this var has a reference to all editors shown on screen
;;;

(defvar *interface-editors* nil)
(defconstant *title-pane-editor* "Editor")
(defconstant *title-pane-graph* "Tree")
(defconstant *title-pane-image* "Graphics")
(defconstant *title-pane-properties* "Properties")
(defconstant *title-pane-parameters* "Parameters")


(defclass pane-editor-entity (base-pane object-with-properties)
  ((model :initarg :model :initform nil :accessor model)
   (interface-mode :initarg :interface-mode :initform nil :accessor interface-mode)
   (property-editors :initarg :property-editors :initform nil :accessor property-editors)
   (edges-weight-values :initarg :edges-weight-values :initform nil :accessor edges-weight-values)
   (image-buffer :initarg :image-buffer :initform nil :accessor image-buffer)
   (image-render-step :initarg :image-render-step :initform 4 :accessor image-render-step)
   (animate :initarg :animate :initform nil :accessor animate)
   (selected-tab :initarg :selected-tab :initform nil :accessor selected-tab)
   (selected-property-tab :initarg :selected-property-tab :initform nil :accessor selected-property-tab)))


(defmethod initialize-properties :after ((p pane-editor-entity))
  "Initialize <p> properties."
  (add-properties-from-values
   p
   (:name 'rezisable :label "Auto adjust size" :accessor-type 'property-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)
   (:name 'pixel-count-x :label "X pixels" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 8 :editor 'integer-editor :visible nil)
   (:name 'pixel-count-y :label "Y pixels" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 8 :editor 'integer-editor :visible nil)
   (:name 'image-render-step :label "Render step" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 2 :editor 'integer-editor)))

(defmethod apply-changes :after ((o pane-editor-entity))
  "Update <o> for any possible change."
  (reset-image o))

(defmethod interface-class ((p pane-editor-entity))
  "Answer <p> interface class."
  (interface-mode p))

(defmethod (setf model) ((p pane-editor-entity) o)
  "Set <o> as <p> model."
  (set-model p o))

(defmethod set-model ((p pane-editor-entity) o)
  "Set <o> as <p> model."
  (setf (slot-value p 'model) o)
  (reset-image-buffer p)
  (set-model (interface p) o))

(defmethod reset-image-buffer ((p pane-editor-entity))
  (setf (image-buffer p) nil))

(defmethod create-property-editors ((p pane-editor-entity))
  "Create property editors for <p>."
  (if (model p) 
      (create-property-editors (interface p))))

(defmethod possible-new-models ((p pane-editor-entity))
  "Answer a list of candidate models for <p>."
  (default-sample-search-objects))

(defmethod set-new-model ((p pane-editor-entity))
  "Set a new model for <p>."
  (let* ((selection (first (prompt-for-default-object-choices "Select an object" (possible-new-models p))))
         (model (if selection (copy (subject selection)))))
    (when model 
      (apply-changes model)
      (set-model p model)
      (trigger (interface p) :interface-model-changed (interface p)))))

(defmethod save-new-model ((p pane-editor-entity))
  "Save <p> model in default objects pool."
  nil)

(defmethod default-search-process ((p pane-editor-entity) model)
  "Answer the default search process for <p>."
  (make-instance 'object-in-search :object model :context (make-instance 'search-task)))

(defmethod initialize-instance :before ((p pane-editor-entity) 
                                        &optional &key mdi-interface model 
                                        (interface-mode 'interface-pane-editor-entity))
  "Initialize <p>."
  (declare (ignore model) (ignore mdi-interface))
  (setf (interface-mode p) interface-mode))

;; #TODO: Move to #'initialize-interface ?
(defmethod initialize-instance :after ((p pane-editor-entity) 
                                       &optional &key mdi-interface model interface-mode)
  "Initialize <p>."
  (declare (ignore mdi-interface) (ignore interface-mode))
  (when (interface p)
    (set-model p model)
    (set-default-editor-tab p)))
 
;; #TODO: Refactor
(defmethod set-default-editor-tab ((p pane-editor-entity))
  "Set default tab for selected object on <p>."
  (let ((object (model p))
        (interface (interface p)))
    (when object
      ;; Special cases because inheritance problems: 'search-task
      (when (or (equal (clos:class-name (class-of object)) 'search-task))
        (set-editor-tab-to interface *title-pane-properties*)
        (return-from set-default-editor-tab t))
      ;; Show graphic if drawablep
      (when (drawablep object)
        (set-editor-tab-to interface *title-pane-image*)
        (return-from set-default-editor-tab t))
      ;; Show tree if drawablep
      (when (drawablep object)
        (set-editor-tab-to interface *title-pane-graph*)
        (return-from set-default-editor-tab t))
      ;; If it's not evolvablep, show properties
      (when (not (evolvablep object))
        (set-editor-tab-to interface *title-pane-properties*)
        (remove-editor-tab interface "Editor")
        (remove-editor-tab interface "Tree")
        (remove-editor-tab interface "Graphics")
        (remove-editor-tab interface "Parameters")
        (return-from set-default-editor-tab t)))))

(defmethod (setf interface-mode) ((p pane-editor-entity) mode)
  "Set <p> interface mode."
  (rebuild-interface p)
  (setf (slot-value p 'interface-mode) mode))

(defmethod rebuild-interface ((p pane-editor-entity))
  "Refresh <p> depending on it's interface mode."
  nil)

(defun remove-editor-tab (interface name)
  (capi:execute-with-interface 
   interface
   (lambda (&rest args)
     (declare (ignore args))
     (let ((editor (editor-tab interface name)))
       (when editor
         (removef (capi:collection-items (tab interface)) 
                  editor
                  :test (lambda (a b) (eql (cadr b) a))))))))

;; Expressions editor interface
(defun children-nodes (list)
  (if (listp list) (cdr list)))

;; #TODO: Temporal code
(defun print-node (node)
  (to-string 
   (if (null node)
       "nil"
     (if (equal "" node)
         ""
       (let ((value (if (listp node) 
                        (car node) 
                      node)))
         (node value))))))

(defun save-interface-tab-indexes (interface)
  (let ((pane (pane interface)))
    (setf (selected-tab pane) (capi:choice-selection (tab interface)))
    ;; #TODO: Unharcode 3!
    (when (= (selected-tab pane) 3)
      (setf (selected-property-tab pane) (capi:choice-selection (capi:pane-layout (second (capi:choice-selected-item (tab interface)))))))))

(defun load-interface-tab-indexes (interface)
  (capi:execute-with-interface 
   interface
   (lambda (&rest args)
     (declare (ignore args))
     (let ((pane (pane interface)))
       (setf (capi:choice-selection (tab interface)) (selected-tab pane))
       (when (= (selected-tab pane) 3)
         (setf (capi:choice-selection (capi:pane-layout (second (capi:choice-selected-item (tab interface))))) 
               (selected-property-tab pane)))))))

;; #TODO: Move from here
(defun apply-editor-changes (interface data)
  "Apply <interface> model changes."
  (declare (ignore data))
  (let* ((pane (pane interface))
         (object (model pane)))
    (save-interface-tab-indexes interface)
    (if (null object) 
        (set-model pane object)
      (progn 
        ;; #TODO: This should be a :program property, refactor
        (if (evolvablep object)
            (setf (program object)
                  (read-from-string (capi:editor-pane-text (pane-editor interface)))))
        (apply-property-values pane)
        (apply-changes object)))
    (ensure-valid-model-properties object)
    (set-model interface object)
    (trigger pane :model-changed)
    (load-interface-tab-indexes interface)))

(defmethod apply-property-values ((p pane-editor-entity))
  "Apply <p> property values changes to it´s model."
  (let ((object (model p)))
    (when (pane-properties (interface p))
      (dolist (editor (property-editors p))
        (update-value editor object)))
    (when (pane-parameters (interface p))
      (dolist (editor (parameter-editors p))
        (update-value editor object)))))

;; #TODO: Ugly hack, parameters should be stored on this pane, not into parameters interface
(defmethod parameter-editors ((p pane-editor-entity))
  "Answer <p> parameter editors."
  (editors (pane-parameters (interface p))))

;; #TODO: Delete interface vars and use pixmap info
(defmethod update-editor-image ((interface base-interface) x y width heigth)
  "Reset image buffer to allow <interface> image update."
  (when (not (and (= (image-heigth interface) heigth)
                  (= (image-width interface) width)))
    (setf (image-heigth interface) heigth
          (image-width interface) width)
    (destroy-interface-pixmap interface)
    (reset-image-buffer (pane interface))))

;; #TODO: Refactor
(defmethod options-menu-description ((pane t) object)
  (options-menu-description-subtasks 
   "Options" 
   (if object 
      (options-menu-description-pane-editor-entity-node pane object)
    options-menu-description-pane-editor-entity)
   pane))

(defmethod options-menu-description ((pane capi:pinboard-layout) object)
  (options-menu-description 
   (capi:element-interface (capi:element-interface pane))
   object))

(defparameter options-menu-description-pane-editor-entity
  '(("Edit"
     ("New" pane-entity-editor-new)
     ("Edit" pane-entity-editor-edit-properties)
     ("Set model" pane-entity-set-new-model)
     ("Save model" pane-entity-save-new-model)
     ("Copy" pane-entity-copy)
     ("Paste" pane-entity-paste)
     ("Import" pane-entity-import)
     ("Export" pane-entity-export)
     ("Send" pane-entity-send)
     ("Open" pane-entity-open-model-new-editor)
     ("Open copy" pane-entity-open-model-copy-new-editor)
     ("Open explorer" pane-entity-open-pane-entity-explorer))
    ("Simplification"
     ("Function" pane-entity-view-simplified-selection-function :disabled)
     ("New search" pane-entity-view-simplified-selection-search))
    ("Local optimizations"
     ("Optimize constants" pane-entity-view-optimized-constants-function))
    ("View"
     ("Detect introns" pane-entity-view-introns)
     ;; #NOTE: Tree / graph statistical analysis
     ;("Intercorrelation" pane-entity-view-selection-intercorrelation :disabled)
     ;("Principal components" pane-entity-view-important-components :disabled)
     )
    ("Search"
     ("Search..." :disabled))))

(defmethod pane-entity-copy ((o t) &optional operation)
  "Add <o> into global *drag-context* selection context."
  (declare (ignore operation))
  (add-to-context *drag-context* :object o))

(defmethod pane-entity-copy ((i redrawing-with-pixmap) &optional operation)
  "Add <i> model into global *drag-context* selection context."
  (declare (ignore operation))
  (add-to-context *drag-context* :object (model (pane (element-interface i)))))

(defun pane-entity-paste (interface data)
  "Set buffered object in *drag-context* as <interface> model."
  (declare (ignore data))
  (set-model 
   (pane (element-interface interface)) 
   (copy (get-from-context *drag-context* :object)))
  (trigger interface :interface-model-changed interface))

(defmethod pane-entity-open-pane-entity-explorer (interface data)
  (open-pane-entity-explorer
   (model (pane (element-interface interface))) 
   interface))

(defmethod options-menu-description-pane-editor-entity-node (pane object)
  "Answer <pane> menu description for <object>."
  '(("Copy subtree " pane-entity-copy-selected-subtree)
    ("Paste subtree" pane-entity-paste-selected-subtree)
    ("Inject expression" pane-entity-inyect-expression)
    ("")
    ("Open new object" pane-entity-inspect-selected-subtree)
    ("")
    ("Operate n -> best" pane-entity-operate-node-best-of-n-times :disabled)
    ("Operate n -> get population" pane-entity-operate-node-n-times-population :disabled)
    ("Simplify b -> best" pane-entity-simplify-n-times-node)
    ("")
    ("Search in new context" pane-entity-search-node-new-process)
    ("Simplify in new context" pane-entity-simplify-node-new-process)
    ("")
    ("Optimize constant" pane-entity-optimize-constants-selected-node)
    ("Parametrize and scan" pane-entity-parametrize-scan-selected-node :disabled)))

;; #TODO: Move this to pane behaviour
(defmethod create-subtree-entity (interface)
  "Answer a new entity with program as selected subtree on <interface>."
  (let ((index (capi:choice-selection (pane-graph interface)))
        (result (copy (model (pane interface)))))
    (setf (program result) (selected-subtree result index))
    result))

(defmethod selected-subtree (object index)
  "Answer a new <object> copy with gene subtree node pointed by <index>."
  (get-internal-subtree 
   (program object) index (language (context object))))

(defmethod pane-entity-inspect-selected-subtree (interface data)
  "Open a new editor with subtree object on <interface>."
  (declare (ignore data))
  (open-in-new-editor (create-subtree-entity interface) interface))

(defmethod pane-entity-copy-selected-subtree (interface data)
  "Copy on clipboard a new object with gene as the selected subtree on <interface>."
  (declare (ignore data))
  (pane-entity-copy (create-subtree-entity interface)))

(defmethod pane-entity-paste-selected-subtree (interface data)
  "Paste on <interface> selected node the subtree on clipboard."
  (declare (ignore data))
  (let* ((index (capi:choice-selection (pane-graph interface)))
         (object (model (pane interface))))
    (setf (program object) 
          (replace-internal-subtree (program object)
                                    (program (get-from-context *drag-context* :object))
                                    index
                                    (language (algorithm (context object)))))
    (set-model interface object)))

(defun normalize-edge-weights (list)
  (let ((acum 0))
    (mapcar (lambda (x) (incf acum (second x))) list)
    (mapcar (lambda (x) (list (first x) (/ (second x) acum) (third x))) list)))


(defclass wiggly-line (capi:line-pinboard-object)
  ())

(defmethod capi:draw-pinboard-object (pinboard (object wiggly-line) &key x y width height &allow-other-keys)
  (declare (ignore x y width height))
  (destructuring-bind
      (start-x start-y end-x end-y)
      (capi::coords-or-geometry object)
    (let* ((dx (- end-x start-x))
           (dy (- end-y start-y))
           (length (sqrt (+ (* dx dx) (* dy dy))))
           (last-x start-x)
           (last-y start-y)
           (wiggles 200)
           (nodes (cdr (flatten (capi:graph-pane-roots pinboard))))
           (drawn-node (slot-value (slot-value (second (slot-value object 'capi::plist)) 'capi::to) 'capi::object))
           (final-object (if (atom drawn-node) (index drawn-node) (index (car drawn-node))))
           (weight-values (normalize-edge-weights (edges-weight-values (pane (capi:element-interface pinboard)))))
           (weight)
           (sign -1))
      (if final-object
          (setf weight 
                (assoc (1- (cadr (assoc final-object 
                                        (let ((i 0))
                                          (mapcar (lambda (x) 
                                                    (incf i)
                                                    (list (index x) i))
                                                  nodes)))))
                       weight-values)))
      (let* ((weight-value (* (if weight (cadr weight) 0) 50))
             (wiggly-x (* (/ dy length) weight-value))
             (wiggly-y (* (/ dx length) (- 0 weight-value))))
        (dotimes (wiggle (floor wiggles))
          (let* ((wiggle-distance (/ (1+ wiggle) wiggles))
                 (next-x (+ start-x (* dx wiggle-distance) (* sign wiggly-x)))
                 (next-y (+ start-y (* dy wiggle-distance) (* sign wiggly-y))))
            (gp:draw-line pinboard last-x last-y next-x next-y)
            (setq last-x next-x
                  last-y next-y
                  sign (- sign))))))))

(defun introns-detection (object-in-search)
  (let* ((object (object object-in-search))
         (context (context object-in-search))
         (algorithm (algorithm context))
         (trees)
         (new-tree)
         (values)
         (subtrees)
         (count-nodes (tree-size (program object)))
         (count-subtrees 50)
         (difference))
    (dotimes (i count-subtrees)
      (push (create-expresion algorithm 4 4 t t) subtrees))
    (dotimes (i count-nodes)
      (setf difference 0)
      (dolist (j subtrees)
        (setf new-tree (replace-internal-subtree (program object) j (1+ i) (language algorithm)))
        (incf difference (evaluate-differences context algorithm object new-tree)))
      (push (list i difference new-tree) values))
    values))

(defun evaluate-differences (context algorithm object new-tree)
  (declare (ignore context))
  (let ((new-object (copy object)))
    (setf (program new-object) new-tree)
    (evaluate algorithm object)
    (evaluate algorithm new-object)
    (abs (- (fitness new-object) (fitness object)))))

(defmethod pane-entity-view-simplified-selection-function (interface data)
  "Open a new editor for an simplification of <interface> model."
  (let* ((object (model (pane interface)))
         (new-object (copy object)))
    (setf (program new-object) 
          (simplify-strategy (program object) nil (language (algorithm (context object)))))
    (open-in-new-editor new-object interface)))

(defmethod pane-entity-simplify-n-times-node (interface data)
  "Simplifies subtree a number of times entered by the user on GUI."
  (let* ((search-object (model (pane interface)))
         (object (object search-object))
         (algorithm (algorithm (context search-object)))
         (steps (prompt-for-plusp-integer "Simplification steps"))
         (index (capi:choice-selection (pane-graph interface)))
         (new-object (copy object)))
    (when steps
      (dotimes (i steps)
        ;; Apply operation
        (setf (program new-object)
              (replace-internal-subtree (program object) 
                                             (simplify-strategy (operate
                                                           (system-get 'branch-delete)
                                                           (language algorithm)
                                                           (list (selected-subtree search-object index)))
                                                          algorithm)
                                             index
                                             (language algorithm)))
        (evaluate algorithm new-object)
        ;; Set new object, if it's better and/or smaller
        (when (or (> (fitness new-object) (fitness object))
                  (and (= (fitness new-object) (fitness object))
                       (< (tree-size (program new-object)) 
                          (tree-size (program object)))))
          (setf (program object) (program new-object)
                (fitness object) (fitness new-object))))
      (set-model interface object))))

(defmethod pane-entity-open-model-copy-new-editor (interface data)
  "Open an editor with a copy of <interface> model."
  (open-in-new-editor 
   (copy-cyclic (model (pane (element-interface interface))))
   interface))

(defmethod pane-entity-open-model-new-editor (interface data)
  "Open an editor with <interface> model."
  (open-in-new-editor
   (model (pane (element-interface interface))) 
   interface))

(defmethod open-in-new-editor ((object t) interface)
  "Open an editor with <object> with mdi-interface as <interface>."
  (let ((editor (make-editor-pane :model object :mdi-interface interface)))
    (set-model editor object)
    (open-pane editor)))

(defun open-pane-entity-explorer (object interface)
  "Open an pane-entity-explorer with <object> as model."
  (let ((editor (make-entity-explorer-pane :model object :mdi-interface interface)))
    (set-model editor object)
    (open-pane editor)))

(defun pane-entity-view-optimized-constants-function (interface data)
  "Open an editor with a constant optimized <interface> model copy."
  (declare (ignore data))
  (let ((new-object (copy (model (pane (element-interface interface))))))
    (execute-optimization-on
     (system-get 'optimization-method-steepest-descent)
     new-object)
    (open-in-new-editor new-object interface)))

(defmethod reset-image ((p pane-editor-entity))
  "Reset cached display image of <p>."
  (setf (image-buffer p) nil))

(defmethod graphic-part ((interface t))
  (pixmap-layout (pane-image interface)))

(defmethod drawable-object ((pane pane-editor-entity))
  "Answer the drawable object of pane."
  (model pane))

(defmethod post-initialize-interface ((p pane-editor-entity))
  "Post initialize actions for <p>."
  (set-model p (model p))
  (when (selected-tab p)
    (set-editor-tab-to (interface p) (selected-tab p))))

(defmethod source-description :before ((p pane-editor-entity))
  "Answer <p> source code description."
  (let* ((interface (interface p))
         (tab (tab interface)))
    (when tab
      (setf (selected-tab p) (editor-label interface (capi:choice-selection tab))))))

