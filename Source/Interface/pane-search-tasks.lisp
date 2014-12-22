(defparameter *default-pane-tasks-default-model* nil)


(defclass pane-search-tasks (pane-subtasks)
  ())


(defmethod initialize-instance :after ((p pane-search-tasks) &optional &key mdi-interface (open t))  
  "Initialize <p>."
  (declare (ignore mdi-interface) (ignore open))
  (setf (model p) (make-new-default-task p)))

(defmethod initialize-properties :after ((i interface-pane-search-tasks))
  "Initialize <p> properties."
  (add-properties-from-values
   i
   (:name 'property-column-list :label "Columns" :accessor-type 'accessor-accessor-type :data-type 'list
    :default-value '(name state best-fitness progress-indicator running-time) :editor 'check-list-editor
    :possible-values '(name state best-fitness progress-indicator running-time))))

(defmethod initialize-timer ((p pane-search-tasks))
  "Initialize <p> timer."
  (setf (timer p) (mp:make-timer (lambda () (safe-refresh-subtasks p))))
  (mp:schedule-timer-milliseconds (timer p) 100 (configuration-get interface-timer-refresh-rate)))

(defmethod initialize-interface :after ((p pane-search-tasks))
  (initialize-timer p))

(defmethod safe-refresh-subtasks ((p pane-search-tasks))
  "Refresh <p> subtasks in another mp:process."
  (mp:process-run-function "Refresh tasks" nil (lambda () (refresh-subtasks p))))

(defmethod make-new-default-task ((p pane-search-tasks))
  "Answer a new default search default task instance."
  (make-instance 'search-task))

(defmethod interface-class ((p pane-search-tasks))
  "Answer <p> interface class."
  'interface-pane-search-tasks)

(defparameter menu-description-search-tasks
  '(("Execution"
     ("Pause" menu-stop-selection lambda-has-selection)
     ("Continue" menu-resume-selection lambda-has-selection)
     ("Delete" menu-delete-selection lambda-has-selection)
     ("Reports" menu-pane-task-report lambda-has-selection))
    ("Task"
     ("Edit" menu-edit-selection lambda-has-selection)
     ("See processes" menu-open-subtasks-editor lambda-has-selection)
     ("Save task" menu-save-selection lambda-has-selection)
     ("Load task" menu-load-task)
     ("Load task template" menu-load-model)
     ("Save task template" menu-save-model)
     ("Load default task" menu-load-default)
     ("Save default task" menu-save-default))
    ("Other"
     ("Objects"
      ("Best" menu-best-task-individual lambda-has-best-individual)
      ("N best" menu-n-best-task-individuals lambda-has-best-individual))
     ("Graphs"
      ("Fitness map" menu-population-fitness-map lambda-has-selection)
      ("Size map" menu-population-size-map lambda-has-selection)
      ("Property over time" menu-property-vs-time :disabled)
      ("Operators usage" menu-operator-usage :disabled)
      ("Functions usage" menu-function-usage :disabled)
      ("Constants usage" menu-constant-usage :disabled)))))


(capi:define-interface interface-pane-search-tasks (interface-pane-subtasks)
  ((sortable-lp-reverse :initform nil)
   (property-column-list :initform nil :accessor property-column-list))
  (:panes
   (subtasks capi:multi-column-list-panel
             :items nil
             :selection-callback 'select-subtask
             :action-callback 'menu-open-subtasks
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
                            menu-description-search-tasks
                            pane))))
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 2 :selected-image 2
                                :help-key "Create task"
                                :selection-callback 'menu-create-task
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 1 :selected-image 1
                                :help-key "Delete all tasks"
                                :selection-callback 'menu-delete-all-tasks
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 8 :selected-image 8
                                :help-key "Edit model task"
                                :selection-callback 'menu-edit-default-subtask
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 9 :selected-image 9
                                :help-key "Pane properties"
                                :selection-callback 'menu-pane-subtasks-properties
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 11 :selected-image 11
                                :help-key "Tasks creator"
                                :selection-callback 'menu-pane-tasks-creator
                                :callback-type :interface-data))
           :selection nil))
    :callback-type :interface-data
    :title "Actions"
    :title-position :frame
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 6)))
  (:layouts
   (main-layout capi:column-layout '(subtasks toolbar))
   (toolbar capi:column-layout '(simple-toolbar)))
  (:default-initargs 
   :best-width 280 :best-height 150 
   :visible-min-width 100 :visible-min-height 100
   :title "Search tasks"
   :destroy-callback 'destroy))


(defmethod set-multi-column-items-tasks ((i interface-pane-search-tasks) list label)
  (set-multi-column-items (interface-model-instance i) list label))

(defmethod interface-model-instance ((i interface-pane-search-tasks))
  *default-instance-search-task*)

(defmethod default-column-names ((i interface-pane-search-tasks))
  '(name state best-fitness progress-indicator running-time))

(defmethod model-copy ((pane pane-search-tasks))
  "Answer a copy of <pane> model."
  (copy-cyclic (model pane)))

(defmethod set-tasks ((pane pane-search-tasks) tasks)
  "Answer a copy of <pane> model."
  (let ((interface (interface pane)))
    ;; Add task to global tasks list
    (setf (elements pane) tasks)
    ;; Update interface
    (capi:apply-in-pane-process
     (subtasks interface) 
     #'(setf capi:collection-items) 
     tasks 
     (subtasks interface))))

(defun menu-create-task (interface data)
  "Create and register a new task on <interface>."
  (declare (ignore data))
  (let* ((pane (pane interface))
         (task (model-copy pane)))
    ;; Add task to global tasks list
    (appendf (elements pane) (list task))
    ;; Update interface
    (capi:apply-in-pane-process
     (subtasks interface) 
     #'(setf capi:collection-items) 
     (elements pane) 
     (subtasks interface))
    ;; #LOG: Prepare for benchmarking
    (prepare-benchmark (make-instance 'task-benchmark) task)
    ;; Execute task with task planifier
    (execute-task (task-planifier task) task)))

(defun menu-create-task-n (interface data)
  "Ask user for number of tasks to create and run."
  (let ((n (prompt-for-plusp-integer "Tasks count:")))
    (if n (dotimes (i n)
            (menu-create-task interface data)))))

(defmethod menu-open-subtasks (data interface)
  "Open subtasks for selected task on <interface>."
  (let ((selected-task (capi:choice-selected-item (subtasks interface))))
    (when selected-task
      (make-instance 'pane-subtasks
                     :mdi-interface interface
                     :model selected-task
                     :container (make-instance 'task-container :name 'selected-task-subtasks :elements (children selected-task))))))

(defmethod model-file-extension ((pane pane-search-tasks))
  "*.task")

(defmethod model-file-extension-description ((pane pane-search-tasks)) 
  "Task file")

(defmethod default-model-path ((pane pane-search-tasks))
  *default-pane-tasks-default-model*)

(defun menu-edit-selection (interface subtasks)
  "Open a new pane-entity-editor with selected object on <interface>."
  (declare (ignore subtasks))
  (let ((selection (selection interface)))
	(when selection (open-editor-with interface selection))))

(defun menu-open-subtasks-editor (interface subtasks)
  "Open subtasks editor for <interface>."
  (menu-open-subtasks subtasks interface))

(defmethod reset-task-planifier-settings ((pane pane-search-tasks))
  (declare (ignore pane))
  (reset-task-environment-settings))

(defun reset-task-environment-settings ()
  (setf *simultaneous-processes* 0
        *target-task-assignment* nil)
  ;; Delete all task processes
  (dolist (process (mp:list-all-processes))
    (if (or (equal "Process task" (mp:process-name process))
            (and (>= (length (mp:process-name process)) (length "Task dispatcher "))
                 (equal "Task dispatcher " (subseq (mp:process-name process) 0 16))))
        (mp:process-kill process)))
  ;; #NOTE: Can be deleted for runtime, this is just for development
  (when (null *task-planifier-lock*) 
    (setf *task-planifier-lock* (mp:make-lock :name "task-planifier-lock")))
  (when (null *auxiliar-lock*) 
    (setf *auxiliar-lock* (mp:make-lock :name "auxiliar-lock"))))

(defmethod save-model ((pane pane-search-tasks))
  (let ((path (capi:prompt-for-file "Select a file to save model"
                                    :filter (model-file-extension pane)
                                    :operation :save
                                    :filters (list (model-file-extension-description pane) 
                                                   (model-file-extension pane)))))
    (when path
      (save-default-model-to pane path))))

(defmethod load-model ((pane pane-search-tasks))
  (let ((path (capi:prompt-for-file "Select a file to load model"
                                    :filter (model-file-extension pane)
                                    :operation :open
                                    :filters (list (model-file-extension-description pane) 
                                                   (model-file-extension pane)))))
    (when path
      (load-default-from pane path))))

(defun menu-n-best-task-individuals (interface data)
  "Open a new pane-buffer with best individuals from <interface> selection."
  (declare (ignore data))
  (let ((selected-task (selection interface)))
    (when selected-task
      (let ((quantity (capi:prompt-for-integer "Individuals: ")))
        (if quantity
            (let ((buffer (make-instance 'pane-buffer  
                                         :population (pane-search-tasks-n-best selected-task quantity)
                                         :mdi-interface (interface (pane interface)))))
              (refresh-images buffer)
              (open-pane buffer)))))))

(defun menu-best-task-individual (interface data)
  "Open a new pane-editor-entity with best individual of <interface> selection."
  (declare (ignore data))
  (when (selection interface)
    (open-editor-with interface 
                      (multiple-value-bind (best subtask)
                          (best-individual (selection interface))
                        (make-instance 'object-in-search :object best :context subtask)))))

;; #TODO: #HARDCODE, this should check if the process has been initialized
(defmethod pane-search-tasks-n-best ((task search-task) n)
  "Answer a new population with the <n> best individuals of <task>."
  (let ((elites)
        (result (make-instance 'population)))
    (dolist (subtask (children task))
      (if (population (algorithm subtask))
          (appendf elites 
                   (mapcar 
                    (lambda (o) (make-instance 'object-in-search :object o :context subtask))
                    (individuals (population (algorithm subtask)))))))
    (setf (individuals-array result) (to-array elites)
          (individuals-array result) (to-array (best-individuals result n)))
    result))

;; #TODO: Lock process to be saved / saving process, prventing problems when executing while saving
(defmethod save-selection ((pane pane-search-tasks))
  (let ((selection (selection (interface pane))))
    (if selection
        (let ((path (capi:prompt-for-file "Select file to save"
                                          :filter (model-file-extension pane)
                                          :operation :save
                                          :filters (list (model-file-extension-description pane) 
                                                         (model-file-extension pane)))))
          (when path
            (if (probe-file path) (delete-file path))
            (when path
              (setf (subtasks selection) nil)
              (save-source-description selection path)))))))

(defun load-task (interface)
  (let* ((pane (pane interface))
         (path (capi:prompt-for-file "Select file to load"
                                     :filter (model-file-extension pane)
                                     :operation :open
                                     :filters (list (model-file-extension-description pane) 
                                                    (model-file-extension pane)))))
    (when path
      ;; Add task to global tasks list
      (appendf (elements pane) (list (load-object-from path))))))

(defun menu-load-task (interface data)
  (declare (ignore data))
  (load-task interface))
