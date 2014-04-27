

(defparameter *pane-explorer-default-parents-number* 8)
(defparameter *pane-explorer-default-children-number* 56)
(defparameter *pane-explorer-x-children-number* 10)
(defparameter *pane-explorer-y-children-number* 6)
(defparameter *pane-explorer-x-parents-number* 6)
(defparameter *pane-explorer-y-parents-number* 1)
(defparameter *default-configuration-path-pane-explorer* nil)


(defclass pane-explorer (base-pane object-with-properties)
  ((number-parents :initarg :number-parents :accessor number-parents)
   (number-children :initarg :number-children :accessor number-children)
   (parents :initarg :parents :initform nil :accessor parents)
   (children :initform nil :accessor children)
   (history :initarg :history :initform nil :accessor history)
   (index-history :initarg :index-history :initform 0 :accessor index-history)
   (algorithm :initarg :algorithm :accessor algorithm)
   (render-step :initarg :render-step :accessor render-step)
   (pane-feedback :initarg :pane-feedback :initform nil :accessor pane-feedback)
   ;; #TODO: Refactor
   (check-genotype :initarg :check-genotype :initform nil :accessor check-genotype)
   (check-phenotype :initarg :check-phenotype :initform nil :accessor check-phenotype)
   (fail-iterations :initarg :fail-iterations :accessor fail-iterations)))

(defmethod initialize-instance :after ((p pane-explorer) &key key)
  "Initialize <p>."
  (declare (ignore key))
  (setf (slot-value p 'parents) 
        (make-instance 'population :count-individuals (number-parents p))
        (slot-value p 'children) 
        (make-instance 'population :count-individuals (number-children p)))
  (load-default-configuration p)
  (remap-interface-population (interface p))
  (refresh-images p)
  (connect-interface p))

(defmethod make-default-manual-search-pane-algorithm ()
  (make-instance 'manual-search-algorithm :context (make-instance 'search-task)))

(defmethod load-default-configuration ((p pane-explorer))
  "Load default configuration from file (if exists) on <p>."
  (if (and *default-configuration-path-pane-explorer*
           (probe-file *default-configuration-path-pane-explorer*))
      (let ((property-list (eval (read-from-string 
                                  (car (load-from-file
                                        *default-configuration-path-pane-explorer*
                                        :tag "#property-list"))))))
        (setf (algorithm p) (first property-list)
              (check-genotype p) (third property-list)
              (check-phenotype p) (fourth property-list)
              (render-step p) (fifth property-list)))))

(defmethod interface-class ((p pane-explorer))
  "Answer <p> interface class."
  'interface-pane-explorer)

(defmethod post-initialize-interface :after ((p pane-explorer))
  "Post initialize actions for <p>."
  nil)

(defmethod initialize-properties :after ((p pane-explorer))
  "Initialize <p> properties."
  (add-properties-from-values
   p
   ;; Population breeding algorithm
   (:name 'algorithm :label "Search algorithm" :accessor-type 'accessor-accessor-type 
    :data-type 'base-model :default-value (make-default-manual-search-pane-algorithm)
    :editor 'button-editor)
   ;; Individuals generation properties
   (:name 'check-unique-genotype :label "Check unique genotype" 
    :accessor-type 'property-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor)
   (:name 'check-unique-phenotype :label "Check unique phenotype" 
    :accessor-type 'property-accessor-type :data-type 'boolean :default-value nil :editor 'boolean-editor)
   (:name 'condicion-a-cumplir :label "Condition" :accessor-type 'property-accessor-type 
    :data-type 'list :default-value t :editor 'lisp-editor)
   (:name 'fail-iterations :label "Fail iterations" 
    :accessor-type 'accessor-accessor-type :data-type 'integer :default-value 50 :editor 'integer-editor)
   ;; Pane properties
   (:name 'number-parents :label "Parents count" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value *pane-explorer-default-parents-number* :editor 'integer-editor)
   (:name 'number-children :label "Children count" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value *pane-explorer-default-children-number* :editor 'integer-editor)
   (:name 'history-level :label "History level" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 1 :editor 'integer-editor)
   ;; Pane children visualization properties
   (:name 'rezisable :label "Auto adjust size" :accessor-type 'property-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)
   (:name 'pixel-count-x :label "X pixels" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 8 :editor 'integer-editor)
   (:name 'pixel-count-y :label "Y pixels" :accessor-type 'property-accessor-type 
    :data-type 'integer :default-value 8 :editor 'integer-editor)
   (:name 'render-step :label "Render step" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 2 :editor 'integer-editor)))

(defun pane-explorer-rebuild-editors (i children parents)
  "Prepare <i> population."
  (capi:apply-in-pane-process
   i
   #'(setf capi:layout-description)
   (image-editors parents)
   (pane-parents i))
  (capi:apply-in-pane-process
   i
   #'(setf capi:layout-description)
   (image-editors children)
   (pane-children i)))

(defmethod apply-changes :after ((o pane-explorer))
  "Update <a> for any possible change."
  (let ((interface (interface o))
        (children (children o))
        (parents (parents o)))
    (update-editor-properties interface)
    (ensure-population-size children (get-value-for-property-named o 'number-children))
    (ensure-population-size parents (get-value-for-property-named o 'number-parents))
    (pane-explorer-rebuild-editors interface children parents)
    (remap-interface-population interface)
    (refresh-images o)))

(capi:define-interface interface-pane-explorer (base-interface)
  ()
  (:panes
   ;; Operations
   (label-operations capi:title-pane :text "Operations" :visible-min-width 120)
   ;; Manual search actions
   (label-search-options capi:title-pane :text "Search actions")
   (button-operate capi:push-button 
                   :text "Generate" :visible-min-width 120 :callback #'operate-callback 
                   :callback-type :interface-data)
   (button-simplify-1 capi:push-button :text "Simplify 1" :visible-min-width 120)
   (button-simplify-2 capi:push-button :text "Simplify 2" :visible-min-width 120 :enabled nil)
   ;; Navitation actions
   (label-navigation capi:title-pane :text "Navigation")
   (button-back capi:push-button :text "<" :callback-type :interface-data
                :callback (lambda (interface data) 
                            (declare (ignore data))
                            (back (pane interface))))
   (button-next capi:push-button :text ">" :callback-type :interface-data
                :callback (lambda (interface data) 
                            (declare (ignore data))
                            (next (pane interface))))
   ;; Pane properties
   (label-edit-properties capi:title-pane :text "Pane actions")
   (button-edit-properties capi:push-button :text "Edit properties" :visible-min-width 120  
                           :callback #'pane-explorer-edit-properties :callback-type :interface-data)
   (button-edit-language capi:push-button :text "Edit language" :visible-min-width 120  
                         :callback #'pane-explorer-edit-language :callback-type :interface-data)
   (button-set-default capi:push-button :text "Set as default" :visible-min-width 120  
                       :callback #'pane-explorer-set-default :callback-type :interface-data)
   (button-save-pane capi:push-button :text "Save pane" :visible-min-width 120  
                     :callback #'pane-explorer-save-pane :callback-type :interface-data)
   (button-load-pane capi:push-button :text "Load pane" :visible-min-width 120  
                     :callback #'pane-explorer-load-pane :callback-type :interface-data)
   ;; Progress bar for long calculations
   (progress-bar capi:progress-bar :visible-min-width 800)
   (pane-parents capi:grid-layout 
                 :description 
                 (loop for i below *pane-explorer-default-parents-number* collect 
                       (interface (make-image-editor-pane :open nil)))
                 :columns 8
                 :accessor pane-parents
                 :title "Parents"
                 :title-position :top)
   (pane-children capi:grid-layout 
                  :description 
                  (loop for i below *pane-explorer-default-children-number* collect 
                        (interface (make-image-editor-pane :open nil))) 
                  :columns 8
                  :accessor pane-children
                  :title "Children"
                  :title-position :top))
  (:layouts 
   (main-layout capi:column-layout '(principal-layout progress-bar))
   (principal-layout capi:row-layout '(principal-sub-layout))
   (population-layout capi:column-layout '(pane-parents nil pane-children))
   (principal-sub-layout capi:column-layout '(rest))
   (operations-layout capi:column-layout '(label-operations) :accessor operations-layout)
   (options-layout capi:column-layout '(label-search-options
                                        button-operate
                                        button-simplify-1 
                                        button-simplify-2))
   (navigation-layout capi:row-layout '(button-back button-next))
   (properties-layout capi:column-layout '(label-edit-properties 
                                           button-edit-properties 
                                           button-edit-language
                                           button-set-default
                                           button-save-pane 
                                           button-load-pane))
   (rest capi:row-layout '(options population-layout))
   (options capi:column-layout '(operations-layout 
                                 options-layout 
                                 properties-layout 
                                 navigation-layout)
            :gap 20))
  (:default-initargs 
   :visible-min-height 630 :visible-min-width 800 
   :title "Crossover editor"
   :destroy-callback 'destroy-interface))

(defmethod destroy-interface ((interface interface-pane-explorer))
  "Perform actions when <interface> is destroyed."
  (let ((children (children (pane interface)))
        (parents (parents (pane interface)))
        (pane-parents (pane-parents interface))
        (pane-children (pane-children interface)))
    (when parents
      (dotimes (i (count-individuals parents))
        (destroy-interface (nth i (capi:layout-description pane-parents)))))
    (when children
      (dotimes (i (count-individuals children))
        (destroy-interface (nth i (capi:layout-description pane-children)))))))

(defmethod pane-explorer-edit-properties (interface data)
  "Edit <interface> pane properties in a new editor."
  (open-in-new-editor (pane interface) interface))

(defmethod pane-explorer-edit-language (interface data)
  "Edit <interface> pane language properties in a new editor."
  (open-in-new-editor (language (algorithm (pane interface))) interface))

(defmethod pane-explorer-save-pane (interface data)
  (prompt-and-save (pane interface)))

(defmethod pane-explorer-load-pane (interface data)
  (prompt-and-load (pane interface)))

(defmethod prompt-and-save ((pane pane-explorer))
  "Open a dialog to save <pane> to a specified file."
  (let ((file (pane-explorer-prompt-file pane :save)))
    (when file
      (if (probe-file file) (delete-file file))
      (remap-population-interface (interface pane))
      (save-source-description (individuals (parents pane)) file :tag "#parents")
      (save-source-description (individuals (children pane)) file :tag "#children"))))

(defmethod prompt-and-load ((pane pane-explorer))
  "Open a dialog to load <pane> from a specified file."
  (let ((file (pane-explorer-prompt-file pane :open)))
    (when file
      (load-in-parents pane (load-from-file file :tag "#parents"))
      (load-in-children pane (load-from-file file :tag "#children"))
      (remap-interface-population (interface pane)))))

(defmethod pane-explorer-prompt-file (pane operation)
  (declare (ignore pane))
  (capi:prompt-for-file 
   "Select file to load"
   :filter "*.crossover-pane"
   :operation operation
   :filters `("Crossover pane files" "*.crossover-pane")))

(defmethod pane-explorer-set-default (interface data)
  "Set <interface> pane as default."
  (let ((pane (pane interface)))
    ;; Delete file if exists
    (if (probe-file *default-configuration-path-pane-explorer*)
        (delete-file *default-configuration-path-pane-explorer*))
    ;; Save properties
    (save-source-description 
     (list 
      (algorithm pane)
      (check-genotype pane)
      (check-phenotype pane) 
      (render-step pane))
     *default-configuration-path-pane-explorer* 
     :tag "#property-list")))

(defmethod load-in-children ((p pane-explorer) objects)
  "Load <objects> on <p> children."
  (let ((children (eval (read-from-string (car objects))))
        (index 0))
    (dolist (object children)
      (setf (aref (individuals-array (children p)) index) object)
      (incf index))))
        
(defmethod load-in-parents ((p pane-explorer) objects)
  "Load <objects> on <p> parents."
  (let ((parents (eval (read-from-string (car objects))))
        (index 0))
    (dolist (object parents)
      (setf (aref (individuals-array (parents p)) index) object)
      (incf index))))
    
;; #TODO: Refactor this function and the next
(defmethod remap-population-interface ((interface interface-pane-explorer))
  "Refresh pane objects from <interface>.
   #NOTE: Refactorize."
  (let ((children (children (pane interface)))
        (parents (parents (pane interface)))
        (pane-parents (pane-parents interface))
        (pane-children (pane-children interface)))
    (dotimes (i (count-individuals children))
      (setf (aref (individuals-array children) i) 
            (model (pane (nth i (capi:layout-description pane-children))))))
    (dotimes (i (count-individuals parents))
      (setf (aref (individuals-array parents) i) 
            (model (pane (nth i (capi:layout-description pane-parents))))))))

(defmethod remap-interface-population ((interface interface-pane-explorer))
  "Refresh pane objects from <interface>.
   #NOTE: Refactorize."  
  (let ((children (children (pane interface)))
        (parents (parents (pane interface)))
        (pane-parents (pane-parents interface))
        (pane-children (pane-children interface)))
    (dotimes (i (count-individuals children))
      (set-model (pane (nth i (capi:layout-description pane-children))) 
                 (aref (individuals-array children) i)))
    (dotimes (i (count-individuals parents))
      (set-model (pane (nth i (capi:layout-description pane-parents))) 
                 (aref (individuals-array parents) i)))))

(defmethod update-editor-properties ((interface interface-pane-explorer))
  "Refresh pane objects FROM <interface>."
  (let* ((pane (pane interface))
         (children (children pane))
         (parents (parents pane))
         (pane-parents (pane-parents interface))
         (pane-children (pane-children interface)))
    (dotimes (i (count-individuals children))
      (setf (image-render-step (pane (nth i (capi:layout-description pane-children))))
            (render-step pane))
      (reset-image (pane (nth i (capi:layout-description pane-children)))))
    (dotimes (i (count-individuals parents))
      (setf (image-render-step (pane (nth i (capi:layout-description pane-parents))))
            (render-step pane))
      (reset-image (pane (nth i (capi:layout-description pane-parents)))))))

(defmethod operate-callback ((i interface-pane-explorer) data &key operation)
  "#NOTE: Includes population remap behaviour."
  (remap-population-interface i)
  (pane-explorer-operate (pane i) :operation operation)
  (remap-interface-population i)
  (update-editor-properties i))

;; #TODO: Do this when setting an object on the pane editor (there operations could change)
(defmethod prepare-operations ((p pane-explorer) parents)
  "Prepare operations of <p> for <parents>."
  (let ((algorithm (algorithm p)))
    (if (or
         (validate-parent-classes p parents)
         (validate-parent-languages p parents))
        (progn 
          (disable-interface-operations p)
          (show-error p "Can't prepare operations for objects of different classes or with different languages.")
          nil)
      (progn
        (setf (objetive-class (context algorithm)) (class-name (class-of (object (car parents))))
              (language (context algorithm)) (language (algorithm (context (car parents)))))
        (set-defaults-for-objetive algorithm)
        (update-interface-operations p)
        t))))

(defmethod show-error ((p pane-explorer) message)
  nil)

(defmethod validate-parent-classes ((p pane-explorer) parents)
  (> (length (unique-eql (mapcar (lambda (each) (class-of (object each))) parents))) 
     1))

(defmethod validate-parent-languages ((p pane-explorer) parents)
  (> (length (unique 
              (mapcar (lambda (each) (language (context each))) parents)
              #'languages-compatible)) 
     1))

(defmethod pane-explorer-operate ((p pane-explorer) &key operation)
  "Create new children using <p> parents with <operation>."
  (when (individuals (parents-sized-population p))
    (let* ((parents (parents-sized-population p))
           (individuals (individuals parents))
           (prepare-result (prepare-operations p individuals))
           (operation (car (find-if (lambda (each) (equal operation (name (car each))))
                                    (genetic-operations p)))))
      (ensure-objetive-data-initialized (algorithm p))
      (update-language (language (algorithm p)))
      (if (and individuals prepare-result)
          (dotimes (i (count-individuals (children p)))
            (setf (aref (individuals-array (children p)) i) 
                  (pane-explorer-create-new-child p parents :operation operation)))))))
 
(defmethod genetic-operations ((p pane-explorer))
  "Answer the genetic operators of <p>."
  (operators (algorithm p)))

(defmethod parents-sized-population ((p pane-explorer))
  "Answer a new population with <p> individuals."
  (let ((individuals)
        (n 0)
        (result (make-instance 'population)))
    (dotimes (i (count-individuals (parents p)))
      (when (aref (individuals-array (parents p)) i) 
        (incf n)
        (setf individuals (push (aref (individuals-array (parents p)) i) individuals))))
    (setf (count-individuals result) n
          (individuals-array result) (to-array individuals))
    result))
	
(defmethod pane-explorer-create-new-child ((p pane-explorer) possible-parent &key operation)
  "Answer a new individual for <p>.
   #NOTE: - Actually applies simplification function."
  (let* ((algorithm (algorithm p))
         (operation (or operation (select-genetic-operation algorithm)))
         (parents (perform-selection (selection-method algorithm) possible-parent (arity operation)))
         (model (first parents))
         (language (language (algorithm p)))
         (object (make-instance (class-of (object model))))
         (child (make-instance 'object-in-search :object object :context (context model))))
    (setf (context (algorithm p)) (context model))
    (block nil
      (dotimes (i (fail-iterations p))
        (let ((exp (multiple-value-bind (child-1 child-2) 
                       (operate operation language (mapcar (lambda (i) (program i)) parents))
                     child-1)))
          (setf (program (object child))
                (simplify language exp))
          (evaluate algorithm (object child))
          (if (accepted-child p child)
              (return-from nil child))))
      child)))

(defmethod accepted-child ((p pane-explorer) (child object-in-search))
  "Answer wheter <child> is acceptable for <p>."
  (let ((check-genotype (check-genotype p))
        (check-phenotype (check-phenotype p)))
    (not (or (includes-p p child check-genotype check-phenotype)
             (constant-p child check-genotype check-phenotype)))))
  
(defmethod includes-p ((p pane-explorer) (o object-in-search) &optional (check-genotype t) (check-phenotype t))
  "Answer whether <o> already exists on <p>."
  (let ((test-function (lambda (x) 
                         (and x (equivalent 
                                 x 
                                 o 
                                 :check-genotype check-genotype 
                                 :check-phenotype check-phenotype)))))
    (or (find-if test-function (individuals (children p)))
        (find-if test-function (individuals (parents p))))))

(defmethod clear ((p pane-explorer))
  "Delete <p> objects (parents and children)."
  (clear-parents p)
  (clear-children p))

(defmethod clear-parents ((p pane-explorer))
  "Clean <p> parents."
  (setf (parents p) nil))

(defmethod clear-children ((p pane-explorer))
  "Clean <p> children."
  (setf (children p) nil))

(defmethod open-in-new-editor ((object pane-explorer) interface)
  "Open a new pane-editor-entity with <object> as model."
  (let ((editor (make-editor-pane :model object :mdi-interface interface)))
    (set-model editor object)
    (when-send-to editor :model-changed 'update-interface-operations object)
    (open-pane editor)))

(defmethod back ((pane pane-explorer))
  "Switch to previous population in <pane> populations pool."
  (decf (index-history pane))
  (setf (children p) (aref (history p) index-history)))

(defmethod next ((pane pane-explorer))
  "Switch to next population in <pane> populations pool."
  (incf (index-history pane))
  (setf (children pane) (aref (history pane) index-history)))

(defmethod create-population-editors ((p pane-explorer) population)
  "Answer a list of editors for individuals in <population>."
  (let ((result)
        (n (length (individuals-array population))))
    (dotimes (i n)
      (let ((editor (make-image-editor-pane
                     :model (aref (individuals-array population) i)
                     :mdi-interface (interface p)
                     :image-render-step (render-step p)
                     :open nil)))
        (push editor result)))
    result))

(defmethod open-in-new-editor ((object pane-explorer) interface)
  "Open a new editor pane for <object>."
  (let ((editor (make-editor-pane :model object :mdi-interface interface)))
    (set-model editor object)
    (open-pane editor)))

(defmethod update-interface-operations ((p pane-explorer))
  "Set operation buttons on <p>."
  (let ((operations (genetic-operations p)))
    (set-interface-operations p operations)))

(defmethod disable-interface-operations ((p pane-explorer))
  "Disable operation buttons on <p>."
  (set-interface-operations p nil))

(defmethod set-interface-operations ((p pane-explorer) operations)
  "Set <operations> on <p> interface."
  (let ((operations-buttons
         (mapcar 
          (lambda (operation) 
            (make-instance 'capi:push-button 
                           :text (symbol-name (name (car operation)))
                           :visible-min-width 120 
                           :callback (lambda (interface data)   
                                       (operate-callback interface data :operation (name (car operation))))
                           :callback-type :interface-data
                           :enabled (not (< (cadr operation) 0.01))))
          operations)))
    (capi:execute-with-interface
     (interface p)
     (lambda () 
       (setf (capi:layout-description (operations-layout (interface p)))      
             (append (list (make-instance 'capi:title-pane :text "Operations" :visible-min-width 120))
                     operations-buttons))))))

(defmethod connect-interface ((p pane-explorer))
  "Connect pane-interface events for <p>."
  (connect-parent-model-event p))

(defmethod connect-parent-model-event ((p pane-explorer))
  (let* ((interface (interface p))
         (parents (parents p))
         (pane-parents (pane-parents interface)))
    (dotimes (i (count-individuals parents))
      (let ((pane (pane (nth i (capi:layout-description pane-parents)))))
        (when-send-to (interface pane) :interface-model-changed 'parent-model-changed-callback p)))))

(defun parent-model-changed-callback (p editor)
  (declare (ignore editor))
  (remap-population-interface (interface p))
  (prepare-operations p (individuals (parents-sized-population p))))
  
(defmethod refresh-images ((p pane-explorer))
  "Refresh <p> images."
  (adjust-editors-size-interface (interface p)))

(defmethod adjust-editors-size-interface ((i interface-pane-explorer))
  "Adjust <i> editors size."
  (adjust-editors-size-imagenes-parent i)
  (adjust-editors-size-imagenes-children i))

(defmethod adjust-editors-size-imagenes-parent ((i interface-pane-explorer))
  "Adjust <i> parent editors size."
  (adjust-editors-size i (capi:layout-description (pane-parents i))))

(defmethod adjust-editors-size-imagenes-children ((i interface-pane-explorer))
  "Adjust <i> children editors size."
  (adjust-editors-size i (capi:layout-description (pane-children i))))

(defmethod adjust-editors-size ((interface interface-pane-explorer) editors)
  "Adjust <interface> children editors size."
  (let ((j 0))
    (dolist (i editors)
      (adjust-editor-size i j interface)
      (incf j))))

;; #TODO:
(defun adjust-editor-size (editor j interface)
  "Adjust <editor> size."
  nil)

