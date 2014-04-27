;;; Default editor for populations and set of invidivuals. 
;;;
;;; Possible visualization mode (#TODO: #CHECK: This should be in something like a combo box or interface option?):
;;;  'interface-mode-tree
;;;  'interface-mode-graphic
;;;  'interface-mode-radial-structure
;;;  'interface-mode-properties
;;;  'interface-mode-text
;;;  
;;; Pane layout modes: 
;;;   'pane-mode-mdi
;;;   'pane-mode-sdi
;;;   'pane-mode-sdi-editors
;;;   'pane-mode-list
;;;

(defclass pane-buffer (base-pane)
  ((population :initarg :population :initform (make-instance 'population :count-individuals 9) :accessor population)
   (pane-mode :initarg :pane-mode :initform 'pane-mode-list :accessor pane-mode)
   (view-mode :initarg :view-mode :initform 'interface-mode-graphic :accessor view-mode)
   (selection :initarg :selection :initform nil :accessor selection)
   (margin-x :initarg :margin-x :initform 0 :accessor margin-x)
   (margin-y :initarg :margin-y :initform 0 :accessor margin-y)
   (timer :initform nil :accessor timer)))


(defparameter menu-description-pane-buffer
  '(("Population"
     ("Clear" menu-pane-buffer-clear)
     ("Add" menu-pane-buffer-add)
     ("Remove" menu-pane-buffer-delete)
     ("Set children" menu-pane-buffer-set-children-size))
    ("Visualization"
     ("Fit editors" menu-pane-buffer-fit-editors))))

(capi:define-interface interface-pane-buffer (base-interface)
  ((interfaces :initarg :interfaces :initform nil :accessor interfaces)
   (sortable-lp-reverse :initform nil :accessor sortable-lp-reverse)
   (property-column-list :initform nil :accessor property-column-list))
  (:panes
   (pane-population capi:switchable-layout :accessor pane-population :combine-child-constraints t)
   (pane-mode capi:option-pane :items '(images expresions) :title "Mode"
              :selection-callback 'pane-buffer-mode-callback))
  (:layouts
   (main-layout capi:column-layout '(pane-mode pinboard))
   (pinboard capi:simple-pinboard-layout '(pane-population)
             :input-model '((:post-menu in-extend-tree-view-test-menu))))
  (:default-initargs 
   :best-width 290 :best-height 300 
   :title "Buffer"
   ;:visible-min-width 100 :visible-min-height 300
   :display-state :no-iconic))
 
(defun make-test-menu (layout &rest args)
  (declare (ignore (layout args)))
  (make-instance 'capi:menu :items (options-menu-description-subtasks 
                                    "Options" 
                                    menu-description-pane-buffer 
                                    nil)))


(defun in-extend-tree-view-test-menu (layout &rest args)
  (capi:display-popup-menu (apply 'make-test-menu layout args)))

(defmethod initialize-instance :before ((p pane-buffer) &key mdi-interface population pane-mode interface-mode)
  "Initialize <p>."
  (declare (ignore mdi-interface))
  (if population (setf (population p) population))
  (if interface-mode (setf (interface-mode p) interface-mode))
  (if pane-mode (setf (pane-mode p) pane-mode)))

(defmethod initialize-interface :after ((p pane-buffer))
  (initialize-timer p)
  (when (population p) 
    (create-population-editors p (population p))
    (refresh-images p)))
  
(defun image-editors (population)
  (loop for i below (population-size population) collect 
        (interface (make-image-editor-pane :open nil))))

(defun layout-imagenes (population)
  (make-instance 'capi:column-layout
                 :description (list (make-instance 'capi:grid-layout 
                                                   :columns 5
                                                   :description (image-editors population)))))

(defun layout-list (population)
  (make-instance 'capi:multi-column-list-panel
                 :items nil
                 :selection-callback 'select-subtask
                 :action-callback 'pane-buffer-edit-entity
                 :header-args (list :print-function 'string-capitalize
                                    :selection-callback
                                    #'(lambda (interface new-sort-key)
                                        (declare (ignore interface))
                                        (set-multi-column-list-panel-pane-buffer
                                         (make-instance 'entity)
                                         (expressions-pane interface)
                                         new-sort-key
                                         (if (eq (sortable-lp-reverse interface) new-sort-key)
                                             (progn
                                               (setf (sortable-lp-reverse interface) nil)
                                               t)
                                           (progn
                                             (setf (sortable-lp-reverse interface) new-sort-key)
                                             nil)))))
                 :visible-min-width 200
                 :keep-selection-p t))

(defmethod initialize-instance :after ((i interface-pane-buffer) &key population)
  "Initialize <i>."
  (pane-buffer-rebuild-editors i population))

(defun pane-buffer-rebuild-editors (i population)
  "Prepare <i> for <population>."
  (setf (interfaces i) (list (layout-list population) (layout-imagenes population)))
  (setf (capi:capi-object-name (expressions-pane i)) 'expresions
        (capi:capi-object-name (images-pane i)) 'images)
  (initialize-expressions-interface i)
  ;; Set interfaces
  (capi:apply-in-pane-process
   (pane-population i)
   #'(setf capi:layout-description)
   (interfaces i)
   (pane-population i))
  ;; Set current interface
  (capi:apply-in-pane-process
   (pane-population i)
   #'(setf capi:switchable-layout-visible-child)
   (images-pane i)
   (pane-population i)))

(defmethod expressions-pane ((i interface-pane-buffer))
  (first (interfaces i)))

(defmethod images-pane ((i interface-pane-buffer))
  (second (interfaces i)))

(defmethod editors-pane ((i interface-pane-buffer))
  (third (interfaces i)))

(defun pane-buffer-mode-callback (data interface)
  (let* ((interface-population (pane-population interface))
         (children (interfaces interface)))
    (capi:apply-in-pane-process
     interface-population
     #'(setf capi:switchable-layout-visible-child)
     (find-if (lambda (x) (equal (capi:capi-object-name x) data)) 
              children)
     interface-population)))

(defmethod initialize-expressions-interface ((interface interface-pane-buffer))
  (let ((i (expressions-pane interface)))
    (setf (property-column-list interface) (default-column-names interface))
    (set-multi-column-list-panel-pane-buffer (interface-model-instance interface) i "Program")
    (setf (capi::multi-column-list-panel-column-function i)
          (lambda (individual) (get-individual-info individual i)))
    (capi:set-geometric-hint (pane-population interface) :external-max-width :screen-width t)
    (capi:set-geometric-hint (pane-population interface) :external-max-height :screen-height t)
    (setf (capi::list-panel-columns i) (columns-description interface))))

(defmethod refresh-images ((p pane-buffer))
  "Refresh <p> images."
  (refresh-interface-images p (interface p)))
             
(defmethod refresh-interface-images (p (i interface-pane-buffer))
  "Refresh <p> images."
  (remap-population-interface p) 
  (adjust-editors-size-imagenes p))

(defmethod remap-population-pane ((p pane-buffer))
  "Synchronize model population with <p> interface."
  (let* ((population (population p))
         (pane-images (first (capi:layout-description (images-pane (interface p)))))
         (layout-description (capi:layout-description pane-images))
         (length (length layout-description)))
    (ensure-population-size population length)
    (dotimes (i length)
      (let ((selection (model (pane (nth i layout-description)))))
        (when selection
          (setf (aref (individuals-array population) i) selection))))))

(defmethod remap-population-interface ((p pane-buffer))
  "Set population editors on <p>."
  (let ((population (population p))
        (pane-images (first (capi:layout-description (images-pane (interface p))))))
    (dotimes (i (individuals-count population))
      (set-model (pane (nth i (capi:layout-description pane-images)))
                 (aref (individuals-array population) i)))))
  
(defmethod interface-class ((p pane-buffer))
  "Answer <p> interface class."
  'interface-pane-buffer)

(defmethod interface-argument-list ((p pane-buffer))
  "Answer <p> interface argument list."
  (append 
   (interface-arguments p)
   (list :population (population p) :pane p)))

(defmethod refresh-pane-columns ((p pane-buffer) individuals interface)
  (setf (capi::multi-column-list-panel-column-function individuals)
        (lambda (process) (get-individual-info process interface)))
  (setf (capi::list-panel-columns individuals) (columns-description interface)))

(defmethod columns-description ((interface interface-pane-buffer))
  "Answer the columns description of <interface>."
  (loop for i in (property-column-list interface)
        collect 
        (let ((property (property-named (interface-model-instance interface) i)))
          (list :title (label property)
                :adjust (if (equal 'number (data-type property)) :right :left)
                :visible-min-width (list 'character (default-length-for-title interface (label property)))))))

(defmethod interface-model-instance ((interface interface-pane-buffer))
  (make-instance 'entity))

(defmethod default-length-for-title ((interface interface-pane-buffer) title)
  "Answer the default lenght for <title>."
  (let* ((default-lengths '(("Program" . 45)
                            ("Size" . 10)
                            ("Fitness" . 10)))
         (value (assoc title default-lengths :test 'equal)))
    (if value (cdr value) 15)))

(defun get-individual-info (entity pane)
  "Answer property value list of <pane> for <entity>."
  (loop for i in (property-column-list pane)
        collect 
        (let ((property (property-named entity i)))
          (if property
              (get-value-for-property entity property)
            nil))))

(defmethod initialize-timer ((p pane-buffer))
  "Initialize <p> timer."
  (setf (timer p) (mp:make-timer (lambda () (safe-refresh-individuals p))))
  (mp:schedule-timer-milliseconds (timer p) 100 3000000))

(defmethod destroy ((p pane-buffer))
  "Perform actions when destroying <p>."
  (mp:unschedule-timer (timer p)))

(defmethod safe-refresh-individuals ((p pane-buffer))
  "Refresh <p> individuals in a new mp:process."
  (mp:process-run-function "Refresh processes" nil
                           (lambda () (refresh-individuals p))))

(defmethod refresh-individuals ((p pane-buffer))
  "Callback to refresh <p> individuals."
  (let* ((interface (interface p))
         (individuals (expressions-pane interface)))
    ;; Refresh expressions pane
    (capi:execute-with-interface
     (interface p)
     (lambda (&rest args) 
       (declare (ignore args))
       (refresh-pane-columns p individuals interface)
       (let ((old-selection (capi:choice-selected-item individuals)))
         (setf (capi:collection-items individuals) (elements p)
               (capi:choice-selected-item individuals) old-selection))))))

(defmethod elements ((p pane-buffer))
  (if (population p)
      (individuals (population p))))

(defmethod set-multi-column-list-panel-test-items-individuos ((i interface-pane-buffer) list label)
  (set-multi-column-list-panel-pane-buffer (make-instance 'entity) list label))

(defmethod default-column-names ((i interface-pane-buffer))
  '(fitness program))

;; #TODO: Refactor this in a new multi-column-list-panel (better than CAPI)
(defun set-multi-column-list-panel-pane-buffer (instance sortable-lp sort-key &optional reversep)
  (let ((property (property-labeled instance sort-key)))
    (setf (capi:collection-items sortable-lp)
          (sort (capi:collection-items sortable-lp)
                (if (is-numeric property)
                    (if reversep '< '>)
                  (if reversep 'string> 'string<))
                :key #'(lambda (state)
                         (get-value-for-property-labeled state sort-key))))))

(defmethod pane-buffer-edit-entity (data interface)
  "Open a new pane-entity-editor on <interface> selected item."
  (let ((selection (capi:choice-selected-item (expressions-pane interface))))
    (when selection
      (set-model 
	     (make-editor-pane :model selection :mdi-interface (interface *main-pane*)) 
	     selection))))

;;; MDI PANE-BUFFER (not used at the moment)
(capi:define-interface interface-pane-buffer-mdi (capi:document-frame base-interface)
  ()
  (:panes
   (pane-images capi:pinboard-layout :accessor pane-images))
  (:default-initargs 
   :best-width 420 :best-height 75 
   :title "Buffer"
   :visible-min-width 435 :visible-min-height 80
   :display-state :no-iconic))

(defmethod refresh-interface-images (p (i interface-pane-buffer-mdi))
  "Refresh <p> images.
   #NOTE: Here editors should be resized."
  (set-population-editors-mdi p) 
  (adjust-editors-size-mdi p))

(defmethod set-population-editors-mdi ((p pane-buffer))
  "Set population editors on <p>."
  (let ((editors (create-population-editors p (population p))))
    (capi:apply-in-pane-process (pane-images (interface p))
                                #'(setf capi:layout-description)
                                (loop for f in editors collect (interface f))
                                (pane-images (interface p)))))

(defmethod adjust-editors-size-mdi ((p pane-buffer))
  "Adjust <p> editors size."
  (let* ((editors (create-population-editors p (population p)))
         (count (length (individuals (population p))))
         (count-x (ceiling (sqrt count)))
         (count-y (ceiling (sqrt count)))
         (j 0))
    (dolist (editor editors)
      (open-pane editor :mdi-interface (interface p))
      (when t
        (capi:execute-with-interface (interface editor)
                                     'capi:set-top-level-interface-geometry
                                     (interface editor)
                                     :x (+ (margin-x p) (* (mod j count-x) 112))
                                     :y (+ (margin-y p) (* (floor j count-y) 127))
                                     :width 100
                                     :height 100) 
        (incf j)))))

(defun menu-pane-buffer-clear (interface data)
  (clear-buffer (pane interface)))

(defun menu-pane-buffer-delete (interface data)
  (remove-from-buffer (pane interface) object))

(defun menu-pane-buffer-add (interface data)
  (add-to-buffer (pane interface) nil))

(defmethod menu-pane-buffer-set-children-size ((o interface-pane-buffer) data)
  (set-children-size (pane o) nil))

(defmethod set-children-size ((o pane-buffer) object)
  "Delete <object from <o> population."
  ;; #TODO: Delete this, use an event
  (remap-population-pane o)
  (let ((count (prompt-for-plusp-integer "Enter number of individuals")))
    (when count
      (ensure-population-size (population o) count)
      (refresh-images o)
      (pane-buffer-rebuild-editors (interface o) (population o))
      ;; #TODO: Delete this, use an event
      (remap-population-interface o))))

(defmethod add-to-buffer ((p pane-buffer) object)
  "Add <object> to <p> population."
  (add-individual (population p) object)
  (refresh-images p))

(defmethod remove-from-buffer ((p pane-buffer) object)
  "Remove <object> from <p> population."
  (delete-individual (population p) object)  
  (refresh-images p))

(defmethod clear-buffer ((p pane-buffer))
  "Clear <p> model."
  (setf (population p) nil)
  (refresh-images p))

(defmethod menu-pane-buffer-fit-editors ((p pane-buffer))
  "Adjust <p> editors size."
  nil)

(defmethod drop-action ((pane pane-buffer) (p population) &optional action-key)
  "Perform drop actions when dropping <p> on <pane>."
  nil)

(defmethod drop-action ((p pane-buffer) (o entity) &optional action-key)
  "Perform drop actions when dropping <o> on <pane>."
  nil)

(defmethod adjust-editors-size-imagenes ((p pane-buffer))
  "Adjust <p> editors size."
  nil)

