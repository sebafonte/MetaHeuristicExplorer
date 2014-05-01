
(defclass pane-subtasks (base-pane)
  ((model :initarg :model :accessor model)
   (subtasks :initarg :subtasks :initform '(nil) :accessor subtasks)
   (timer :initform nil :accessor timer)))


(defmethod initialize-interface :after ((p pane-subtasks))
  (initialize-timer p))

(defmethod initialize-timer ((p pane-subtasks))
  "Initialize <p> timer."
  (setf (timer p) (mp:make-timer (lambda () (safe-refresh-subtasks p))))
  (mp:schedule-timer-milliseconds (timer p) 100 400))

(defmethod destroy ((p pane-subtasks))
  "Perform actions when destroying <p>."
  (mp:unschedule-timer (timer p)))

(defmethod safe-refresh-subtasks ((p pane-subtasks))
  "Refresh <p> subtasks in another mp:process."
  (mp:process-run-function "Refresh subtasks" nil (lambda () (refresh-subtasks p))))

(defmethod refresh-subtasks ((p pane-subtasks))
  "Callback to refresh <p> subtasks."
  (when (interface p)
    (let* ((interface (interface p))
           (subtasks (subtasks interface))
           (elements (elements p)))
      (capi:execute-with-interface
       interface
       (lambda ()
         (if (equals (to-list (capi:collection-items subtasks)) elements)
             (refresh-subtasks-redraw subtasks)
           (refresh-subtasks-items elements subtasks)))))))

(defun refresh-subtasks-items (elements subtasks)
  (let ((old-selection (capi:choice-selected-item subtasks)))
    (setf (capi:collection-items subtasks) elements
          (capi:choice-selected-item subtasks) old-selection)))

(defun refresh-subtasks-redraw (subtasks)
  (capi:map-collection-items 
   subtasks
   (lambda (item) (capi:redisplay-collection-item subtasks item))))

(defmethod refresh-pane-columns ((p pane-subtasks) subtasks interface)
  (set-multi-column-items-tasks interface subtasks "Name")
  (setf (capi::multi-column-list-panel-column-function subtasks)
        (lambda (task) (get-task-info task interface)))
  (capi::update-representation subtasks)
  (setf (capi::list-panel-columns subtasks)
        (columns-description interface)))
  
(defmethod elements ((p pane-subtasks))
  (cdr (subtasks p)))

(defmethod (setf elements) (value (p pane-subtasks))
  (setf (cdr (subtasks p)) value))

(defmethod interface-class ((p pane-subtasks))
  "Answer <p> interface class."
  'interface-pane-subtasks)

(defparameter menu-description-pane-subtasks
  '(;; Search options
    ("Execution"
     ("Pause" menu-stop-selection lambda-has-selection)
     ("Continue" menu-resume-selection lambda-has-selection)
     ("Delete" menu-delete-selection lambda-has-selection))
     ;; Tasks
     ("Edit"
      ("Save task" menu-save-selection))
     ;; Population inspect
     ("Inspect"
      ("Individuals"
       ("Best" menu-inspect-best-individual lambda-has-best-individual)
       ("N bests" menu-inspect-best-individuals lambda-has-best-individual)
       ("Principal components" menu-principal-components :disabled)
       ("Fitness correlation" menu-fitness-node-intercorrelation :disabled))
      ;; Search properties
      ("Objects"
       ("Population" menu-view-population lambda-has-selection)
       ("Search algorithm" menu-edit-search-algorithm lambda-has-selection)
       ("Task" menu-edit-search-subtask lambda-has-selection))
      ;; Search graphs
      ("Graphs"
       ("Fitness map" menu-population-fitness-map lambda-has-selection)
       ("Size map" menu-population-size-map lambda-has-selection)
       ("Diversity over time" menu-diversity-vs-time :disabled)
       ("Fitness over time" menu-fitness-vs-time lambda-has-selection)
       ("Mean fitness over time" menu-medium-population-fitness-vs-time lambda-has-selection)
       ("Max size over time" menu-max-size-vs-time lambda-has-selection)
       ("Mean size over time" menu-medium-population-size-vs-time lambda-has-selection)
       ("Property over time" menu-property-vs-time :disabled)
       ("Population graph" menu-population-graph lambda-has-selection)
       ("Operators usage" menu-operator-usage :disabled)
       ("Functions usage" menu-function-usage :disabled)
       ("Constants usage" menu-constant-usage :disabled)
       ("Evaluation time" menu-evaluation-times :disabled)
       ("Best individual evolution" menu-best-individual-evolution :disabled)))
     ;; Other
     ("Operations"
      ("Delete completed" menu-delete-completed)
      ("Delete worser than" menu-delete-worser-than))))


(defun lambda-has-best-individual (interface)
  (lambda-has-best-individual-interface (capi:element-interface interface)))

(defmethod lambda-has-best-individual-interface ((interface interface-pane-subtasks))
  (let ((selection (selection interface)))
    (and selection (best-individual selection))))

(defmethod lambda-has-best-individual-interface ((interface interface-pane-search-tasks))
  (let ((selection (selection interface)))
    (and selection (pane-search-tasks-n-best selection 1))))

(defun lambda-has-selection (interface)
  (not (null (selection (capi:element-interface interface)))))
                           
(defun menu-stop-selection (interface data)
  "Suspend selected subtask execution on <interface>."
  (declare (ignore data))
  (if (selection interface) 
      (ejecutar-wait (selection interface))))

(defun menu-resume-selection (interface data)
  "Resume selected subtask execution on <interface>."
  (declare (ignore data))
  (if (selection interface)
      (ejecutar-signal (selection interface))))

(defun menu-save-template (interface data)
  "Save subtask <interface> model."
  (progn nil))

(defun menu-load-template (interface data)
  "Load subtask <interface> model."
  (progn nil))

(defun menu-delete-selection (interface data)
  "Delete selected subtask on <interface>."
  (delete-subtask interface data))

(defun reset-search-subtasks (data interface)
  "Reset selected subtask on <interface>."
  (declare (ignore data))
  (resetear (selection interface)))

;; #TODO: It should open an 'pane-editor-entity maybe (#CHECK)
(defun menu-view-population (interface data)
  "Open a new pane-buffer with <interface> on selected task population."
  (declare (ignore data))
  (open-new-buffer-with-population-individuals
   (individuals (population (algorithm (selection interface))))
   interface))

(defun menu-population-graph (interface data)
  "Open a new pane-buffer with <interface> selected process population graph."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-property-map 
                                      :name "Fitness map"
                                      :subject (population selection)
                                      :property 'fitness
                                      :value-min (min-fitness (fitness-evaluator selection))
                                      :value-max (max-fitness (fitness-evaluator selection)))
              :mdi-interface interface)))))

(defun open-editor-with (interface object)
  "Open a new pane-editor-entity with <object>."
  (let ((editor (make-editor-pane :model object :mdi-interface interface)))
	(set-model editor object)
    (open-pane editor)))

(defun menu-inspect-best-individual (interface data)
  "Open a new pane-editor-entity with best <interface> individual."
  (declare (ignore data))
  (when (selection interface)
    (open-editor-with
     interface 
     (make-instance 'object-in-search
                    :object (best-individual (selection interface))
                    :context (selection interface)))))

(defun menu-edit-search-algorithm (interface data)
  "Open an editor with <interface> pane model subtask algorithm."
  (declare (ignore data))
  (when (selection interface)
    (open-editor-with interface (algorithm (selection interface)))))
  
(defun menu-edit-search-subtask (interface data)
  "Open an editor with <interface> pane selected subtask."
  (declare (ignore data))
  (when (selection interface)
    (open-editor-with interface (selection interface))))

(defun menu-edit-default-subtask (interface data)
  "Open an editor with <interface> pane model subtask."
  (declare (ignore data))
  (open-editor-with interface (model (pane interface))))

(defun menu-pane-subtasks-properties (interface data)
  "Open an editor with <interface> properties."
  (declare (ignore data))
  (open-editor-with interface interface))

(defun menu-inspect-best-individuals (interface data)
  "Open a new pane-buffer with best N individuals of <subtask>, where N asked to user on GUI."
  (declare (ignore data))
  (let ((n (capi:prompt-for-integer "How many: ")))
    (if n
        (open-new-buffer-with-population-individuals
         (best-individuals (population (algorithm (selection interface))) n)
         interface))))

(defun open-new-buffer-with-population-individuals (individuals interface)
  (let* ((selection (selection interface))
         (individuals (selection-individuals-array individuals selection))
         (population (make-instance 'population)))
    (setf (individuals-array population) individuals)
    (let ((buffer (make-instance 'pane-buffer 
                                 :population population
                                 :mdi-interface (interface (pane interface)))))
      (refresh-images buffer)
      (open-pane buffer))))

(defun selection-individuals-array (individuals selection)
  (to-array (mapcar 
             (lambda (o) 
               (make-instance 'object-in-search :object o :context selection))
             individuals)))

(defun menu-fitness-vs-time (interface data)
  "Open a graphic with best individual fitness value vs time."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :name "Fitness vs. time"
                                      :subject selection
                                      :xmin 0 
                                      :xmax (max-generations (algorithm selection)) 
                                      :ymin (min-fitness (fitness-evaluator selection)) 
                                      :ymax (max-fitness (fitness-evaluator selection))
                                      :valuable-x-list '(lambda (o) (value-for o :generation))
                                      :valuable-y-list '(lambda (o) (fitness (value-for o :best-individual)))
                                      :datasource-list '(lambda (o) (log-data-for-criteria (log-data o) :best-individual)))
              :mdi-interface interface)))))

(defun menu-medium-population-fitness-vs-time (interface data)
  "Open a graphic with medium population fitness value vs time."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :name "Med. fitness vs. time"
                                      :subject selection
                                      :xmin 0 
                                      :xmax (max-generations (algorithm selection)) 
                                      :ymin (min-fitness (fitness-evaluator selection))
                                      :ymax (max-fitness (fitness-evaluator selection))
                                      :valuable-x-list '(lambda (o) (value-for o :generation))
                                      :valuable-y-list '(lambda (o) (value-for o :medium-fitness))
                                      :datasource-list '(lambda (o) (log-data-for-criteria (log-data o) :medium-fitness)))
              :mdi-interface interface)))))

(defun menu-population-fitness-map (interface data)
  "Open a graphic with ordered population fitness values."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :name "Fitness distribution"
                                      :subject (population selection)
                                      :xmin 0 
                                      :xmax (count-individuals (population selection)) 
                                      :ymin (min-fitness (fitness-evaluator selection))
                                      :ymax (max-fitness (fitness-evaluator selection))
                                      :valuable-x-list '(lambda (o) (first o))
                                      :valuable-y-list '(lambda (o) (second o))
                                      :datasource-list '(lambda (o) (sorted-property-values-map o 'fitness)))
              :mdi-interface interface)))))

(defun menu-population-size-map (interface data)
  "Open a graphic with ordered population fitness."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :name "Size distribution"
                                      :subject (population selection)
                                      :xmin 0 
                                      :xmax (count-individuals (population selection)) 
                                      :ymin 0 
                                      :ymax (max-size selection)
                                      :valuable-x-list '(lambda (o) (first o))
                                      :valuable-y-list '(lambda (o) (second o))
                                      :datasource-list '(lambda (o) (sorted-property-values-map o 'structure-size)))
              :mdi-interface interface)))))

(defun menu-max-size-vs-time (interface data)
  "Open a graphic of max size of best individual vs. time."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :name "Size of best vs time"
                                      :subject selection
                                      :xmin 0 
                                      :xmax (max-generations (algorithm selection)) 
                                      :ymin 0
                                      :ymax (max-size (algorithm selection))
                                      :valuable-x-list '(lambda (o) (value-for o :generation))
                                      :valuable-y-list '(lambda (o) (structure-size (value-for o :best-individual)))
                                      :datasource-list '(lambda (o) (log-data-for-criteria (log-data o) :best-individual)))
              :mdi-interface interface)))))
  
(defun menu-medium-population-size-vs-time (interface data)
  "Open a graphic of medium population size of algorithm vs. time."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :name "Med. size vs time"
                                      :subject selection
                                      :xmin 0 
                                      :xmax (max-generations (algorithm (selection interface))) 
                                      :ymin 0 
                                      :ymax (max-size (algorithm selection))
                                      :valuable-x-list '(lambda (o) (value-for o :generation))
                                      :valuable-y-list '(lambda (o) (value-for o :medium-size))
                                      :datasource-list '(lambda (o) (log-data-for-criteria (log-data o) :medium-size)))
              :mdi-interface interface)))))
  
(defun menu-diversity-vs-time (interface data)
  "Open a graphic of diversity vs. time."
  (declare (ignore data))
  (let ((selection (selection interface)))
    (when selection
      (open-pane (make-graphic-pane
              :graphic (make-instance 'graphic-function-r-r
                                      :subject selection
                                      :xmin 0 
                                      :xmax (max-generations (algorithm (selection interface)))
                                      :ymin 0 
                                      :ymax 10
                                      :valuable-x-list '(lambda (o) (generation o))
                                      :valuable-y-list '(lambda (o) (second o))
                                      :datasource-list '(lambda (o) (log-data-for-criteria (log-data o) :diversity-measurement)))
              :mdi-interface interface)))))

(defun menu-property-vs-time (interface data)
  "Open a graphic of the value of a property vs. time."
  (declare (ignore data))
  (let ((value (prompt-for-sub-expression "Datasource valuable")))
    (open-pane (make-graphic-pane
            :graphic (make-instance 'graphic-function-r-r
                                    :subject (selection interface)
                                    :xmin 0 
                                    :xmax (max-generations (algorithm (selection interface)))
                                    :ymin 0 
                                    :ymax 10
                                    :valuable-x-list '(lambda (o) (generation o))
                                    :valuable-y-list '(lambda (o) (second o))
                                    :datasource-list '(lambda (o) (fitness o)))
            :mdi-interface interface))))

;; #TODO: Look out because now when finishes it deletes all processes (?)
(defun menu-delete-completed (interface data)
  "Delete all completed subtasks."
  (declare (ignore interface subtask))
  (setf (elements (pane interface))
        (select (elements (pane interface))
                (lambda (each) (not (equal (state each) 'FINISHED))))))

(defun menu-delete-worser-than (interface data)
  "Delete all subtasks on <interface> pane with fitness value under a user prompted value."
  (declare (ignore subtask interface))
  (let ((min-fitness (capi:prompt-for-number "Min fitness: ")))
    (when min-fitness
      (setf (elements (pane interface)) 
            (loop for i in (elements (pane interface)) 
                  if (or (not (equal (state i) 'FINISHED))
                         (>= (fitness (best-individual i)) min-fitness))
                  collect i)))))

(defun get-task-info (task pane)
  "Answer a list with <task> information."
  (loop for i in (property-column-list pane)
        collect 
        (if (property-named task i)
            (get-value-for-property-named task i)
          nil)))

(defmethod default-column-names ((i interface-pane-subtasks))
  '(name state best-fitness best-size progress-indicator))

(defmethod initialize-instance :after ((i interface-pane-subtasks) &key)
  ;; Set properties for default columns
  (setf (property-column-list i) (default-column-names i))
  ;; Set default columns
  (with-slots (subtasks) i
    (set-multi-column-items-tasks i subtasks "Name"))
  ;; Set column value functions
  (setf (capi::multi-column-list-panel-column-function (subtasks i))
        (lambda (subtask)
          (get-task-info subtask i)))
  (setf (capi::list-panel-columns (subtasks i))
        (columns-description i)))
  
(defmethod default-length-for-title ((interface interface-pane-subtasks) title)
  "Answer the default lenght for <title>."
  (let* ((default-lengths '(("Name" . 15)
                            ("State" . 10)
                            ("Fitness" . 8)
                            ("Nodes" . 7)
                            ("Best size" . 10)
                            ("Size" . 6)
                            ("Time" . 6)
                            ("Progress" . 10)
                            ("Generation" . 12)))
         (value (assoc title default-lengths :test 'equal)))
    (if value (cdr value) 15)))

(defmethod interface-model-instance ((interface interface-pane-subtasks))
  *default-instance-search-task*)

(defmethod columns-description ((interface interface-pane-subtasks))
  "Answer the columns description of <interface>."
  (let ((interface-model (interface-model-instance interface)))
    (loop for i in (property-column-list interface)
          collect 
          (let ((property (property-named interface-model i)))
            (list :title (label property)
                  :adjust (if (equal 'number (data-type property)) :right :left)
                  :visible-min-width (list 'character (default-length-for-title interface (label property))))))))

(capi:define-interface interface-pane-subtasks (base-interface object-with-properties)
  ((sortable-lp-reverse :initform nil)
   (property-column-list :initform nil :accessor property-column-list))
  (:panes
   (subtasks capi:multi-column-list-panel
             :action-callback (lambda (a b) (menu-inspect-best-individual b a))
             :items nil
             :selection-callback 'select-subtask
             :accessor subtasks
             :header-args (list :print-function 'string-capitalize
                                :selection-callback
                                #'(lambda (interface new-sort-key)
                                    (set-multi-column-items
                                     (make-instance 'search-task)
                                     (subtasks interface)
                                     new-sort-key
                                     (if (eq sortable-lp-reverse new-sort-key)
                                         (progn
                                           (setf sortable-lp-reverse nil)
                                           t)
                                       (progn
                                         (setf sortable-lp-reverse new-sort-key)
                                         nil))))) 
             :keep-selection-p t
             :pane-menu (lambda (pane object x y) 
                          (make-pane-menu-with-submenus 
                           pane 
                           object 
                           x 
                           y 
                           (options-menu-description-subtasks 
                            "Options" 
                            menu-description-pane-subtasks 
                            pane))))
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 1 :selected-image 1
                                :help-key "Delete all subtasks"
                                :selection-callback 'menu-delete-all-tasks)
                 (make-instance 'capi:toolbar-button :image 9 :selected-image 9
                                :help-key "Pane properties"
                                :selection-callback 'menu-pane-subtasks-properties))
           :selection nil))
    :callback-type :interface-data
    :title "Actions"
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                        :id 'global-button-icons-images
                        :image-count 6)))
  (:layouts
   (main-layout capi:column-layout '(subtasks toolbar))
   (toolbar capi:column-layout '(simple-toolbar)))
  (:default-initargs 
   :best-width 300 :best-height 150 
   :visible-min-width 100 :visible-min-height 100
   :title "Search subtasks"
   :destroy-callback 'destroy))


(defmethod initialize-properties :after ((p interface-pane-subtasks))
  "Initialize <p> properties."
  (add-properties-from-values
   p
   (:name 'property-column-list :label "Columns" :accessor-type 'accessor-accessor-type 
    :default-value '(name state best-fitness best-size progress-indicator) 
    :data-type 'list :editor 'check-list-editor
    :possible-values '(name state best-fitness best-size progress-indicator))))

(defmethod set-multi-column-items-tasks ((i interface-pane-subtasks) list label)
  (set-multi-column-items (interface-model-instance i) list label))

(defmethod destroy ((o interface-pane-subtasks))
  "Perform actions when destroying <o>."
  (destroy (pane o)))

(defmethod selection ((i interface-pane-subtasks))
  "Answer <i> selection."
  (capi:choice-selected-item (subtasks i)))

(defmethod menu-clear-log-data ((i interface-pane-subtasks))
  "Clear log data on <i>."
  nil)

(defmethod select-subtask (data interface) 
  nil)

(defmethod model-copy ((pane pane-subtasks))
  "Answer a copy of <pane> model."
  (copy (model pane)))

(defmethod create-default-task ((pane pane-subtasks))
  "Create a new task on <pane>."
   (make-instance 'search-task 
                  :subtasks (list (model-copy pane))
                  :task-planifier (system-get 'global-running-image-planifier)))

(defun menu-create-search-subtask (interface data)
  "Create a new search subtask on <interface>."
  (declare (ignore data))
  ;; Create a task to execute the subtask
  (let* ((pane (pane interface))
         (task (create-default-task pane)))
    ;; Add subtask to interface
    (capi:apply-in-pane-process
     (subtasks interface) 
     #'(setf capi:collection-items) 
     (elements pane) 
     (subtasks interface))
    ;; Execute task
    (execute-task (task-planifier task) task)))

(defun menu-create-search-subtask-n (interface data)
  "Asks on GUI for subtasks quantity and creates them if user accepted."
  (let ((n (prompt-for-plusp-integer "How many?:")))
    (if n (dotimes (i n)
            (menu-create-search-subtask interface data)))))

(defun delete-subtask (interface data)
  "Delete selected subtask on <interface>."
  (declare (ignore data))
  (let ((selection (selection interface))
        (subtasks (subtasks interface)))
    (when selection
      ;; Kill selected subtask
      (kill-task selection)
      ;; Delete subtask from pane elements list
      (setf (elements (pane interface)) (remove selection (elements (pane interface))))
      ;; Refresh interface
      (capi:apply-in-pane-process
       subtasks #'(setf capi:collection-items) (elements (pane interface)) subtasks))))

(defun menu-delete-all-tasks (interface data)
  "Delete all <interface> tasks."
  (declare (ignore data))
  ;; Kill all subtasks
  (dolist (i (elements (pane interface))) 
    (kill-task i))
  ;; Clear pane subtask list
  (setf (subtasks (pane interface)) (list nil)
        (elements (pane interface)) nil)
  ;; Refresh interface
  (capi:apply-in-pane-process
   (subtasks interface) #'(setf capi:collection-items) nil (subtasks interface))
  ;; Refresh task planifier (necessary here now)
  (reset-task-planifier-settings (pane interface)))

(defun menu-save-default (interface data)
  (declare (ignore data))
  (save-default-subtask (pane interface)))

(defun menu-load-default (interface data)
  (declare (ignore data))
  (load-default (pane interface)))

(defun menu-save-selection (interface data)
  (declare (ignore data))
  (save-selection interface))

(defmethod default-model-path ((pane pane-subtasks))
  *default-pane-subtasks-default-model*)

(defmethod model-file-extension ((pane pane-subtasks))
  "*.subtask")

(defmethod model-file-extension-description ((pane pane-subtasks)) 
  "Subtask file")

(defmethod save-default-subtask ((pane pane-subtasks))
  (let ((path (default-model-path pane)))
    (save-default-model-to pane path)))

(defmethod save-default-model-to ((pane pane-subtasks) path)
  (if (probe-file path) (delete-file path))
  (save-source-description (model pane) path))
  
(defmethod load-default ((pane pane-subtasks))
  (let ((path (default-model-path pane)))
    (load-default-from pane path)))

(defmethod load-default-from ((pane pane-subtasks) path)
  (if (probe-file path)
      (setf (model pane)
            (eval (read-from-string (car (load-from-file path)))))))

(defmethod save-selection (interface)
  (save-selection (pane interface)))

(defmethod save-selection ((pane pane-subtasks))
  (let ((selection (selection (interface pane))))
    (if selection
        (let ((path (capi:prompt-for-file "Save subtask"
                                          :filter (model-file-extension pane)
                                          :operation :save
                                          :filters (list (model-file-extension-description pane) 
                                                         (model-file-extension pane)))))
          (when path
            (if (probe-file path) (delete-file path))
            (if path (save-source-description selection path)))))))

(defun menu-load-model (interface data)
  (declare (ignore data))
  (load-model (pane interface)))

(defun menu-save-model (interface data)
  (declare (ignore data))
  (save-model (pane interface)))

(defmethod reset-task-planifier-settings ((pane pane-subtasks))
  (declare (ignore pane))
  (reset-task-environment-settings))

#|
;; #TODO:
(defun menu-fitness-node-intercorrelation (interface data)
  (declare (ignore subtask interface))
  nil)
|#
