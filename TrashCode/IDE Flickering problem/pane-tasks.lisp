
(defclass pane-tasks (base-pane)
  ((tasks :initarg :tasks :accessor tasks)
   (timer :initarg :timer :accessor timer)))


(defmethod initialize-timer ((p pane-tasks))
  "Initialize <p> timer."
  (setf (timer p) (mp:make-timer (lambda () (safe-refresh-tasks p))))
  (mp:schedule-timer-milliseconds (timer p) 100 400))

(defmethod initialize-interface :after ((p pane-tasks))
  (initialize-timer p))

(defmethod safe-refresh-tasks ((p pane-tasks))
  "Refresh <p> tasks in another mp:process."
  (mp:process-run-function "Refresh tasks" nil (lambda () (refresh-tasks p))))

(defmethod interface-class ((p pane-tasks))
  "Answer <p> interface class."
  'interface-pane-tasks)

(capi:define-interface interface-pane-subtasks (base-interface)
  ((sortable-lp-reverse :initform nil)
   (property-column-list :initform nil :accessor property-column-list))
  (:layouts
   (main-layout capi:column-layout '(tasks toolbar))
   (toolbar capi:column-layout '(simple-toolbar)))
  (:default-initargs 
   :best-width 300 :best-height 150 
   :visible-min-width 100 :visible-min-height 100
   :title "Search tasks"
   :destroy-callback 'destroy))

(capi:define-interface interface-pane-tasks (interface-pane-subtasks)
  ((sortable-lp-reverse :initform nil)
   (property-column-list :initform nil :accessor property-column-list))
  (:panes
   (tasks capi:multi-column-list-panel
          :items nil
          :selection-callback 'select-subtask
          :action-callback 'menu-open-tasks
          :accessor tasks
          :header-args (list :print-function 'string-capitalize
                             :selection-callback
                             #'(lambda (interface new-sort-key)
                                 (set-multi-column-list-panel-test-items
                                  (make-instance 'search-task)
                                  (tasks interface)
                                  new-sort-key
                                  (if (eq sortable-lp-reverse new-sort-key)
                                      (progn
                                        (setf sortable-lp-reverse nil)
                                        t)
                                    (progn
                                      (setf sortable-lp-reverse new-sort-key)
                                      nil)))))
          :keep-selection-p t
          :pane-menu nil)
   (button-create capi:push-button :text "Create" :callback 'menu-create-task :callback-type :interface-data)
   (button-create-n capi:push-button :text "Create N" :callback 'menu-create-task-n :callback-type :interface-data)
   (button-delete-all capi:push-button :text "Delete all" :callback 'menu-delete-all-tasks :callback-type :interface-data)
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
                                :callback-type :interface-data))
           :selection nil))
    :callback-type :interface-data
    :title "Actions"
    :title-position :frame
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 6)))
  (:layouts
   (main-layout capi:column-layout '(tasks toolbar))
   (toolbar capi:column-layout '(simple-toolbar)))
  (:default-initargs 
   :best-width 280 :best-height 150 
   :visible-min-width 100 :visible-min-height 100
   :title "Search tasks"
   :destroy-callback 'destroy))

(defun menu-create-task (interface data)
  "Create and register a new task on <interface>."
  (declare (ignore data))
  (let* ((pane (pane interface))
         (task (model-copy pane)))
    ;; Add task to global tasks list
    (appendf (elements pane) (list task))
    ;; Update interface
    (capi:apply-in-pane-process
     (tasks interface) 
     #'(setf capi:collection-items) 
     (elements pane) 
     (tasks interface))
    (prepare-benchmark (make-instance 'task-benchmark) task)
    ;; Execute task with task planifier
    (execute-task (task-planifier task) task)))

(defun menu-create-task-n (interface data)
  "Ask user for number of tasks to create and run."
  (let ((n (prompt-for-plusp-integer "Tasks count:")))
    (if n (dotimes (i n)
            (menu-create-task interface data)))))

(defun menu-edit-selection (interface tasks)
  "Open a new pane-entity-editor with selected object on <interface>."
  (declare (ignore tasks))
  (let ((selection (selection interface)))
	(when selection (open-editor-with interface selection))))

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
  (if (null *task-planifier-lock*) 
      (setf *task-planifier-lock* (mp:make-lock))))

(defmethod refresh-tasks ((p pane-tasks))
  "Callback to refresh <p> tasks."
  (when (interface p)
    (let* ((interface (interface p))
           (tasks (tasks interface))
           (elements (tasks p)))
      (capi:execute-with-interface
       interface
       (lambda ()
         (if (equals (to-list (capi:collection-items tasks)) elements)
             (refresh-tasks-redraw tasks)
           (refresh-tasks-items elements tasks)))))))

(defun refresh-tasks-items (elements tasks)
  (let ((old-selection (capi:choice-selected-item tasks)))
    (setf (capi:collection-items tasks) elements
          (capi:choice-selected-item tasks) old-selection)))

(defun refresh-tasks-redraw (tasks)
  (capi:map-collection-items 
   tasks
   (lambda (item) (capi:redisplay-collection-item tasks item))))

(defmethod destroy ((i interface-pane-tasks))
  "Perform actions when destroying <p>."
  (destroy (pane i)))

(defmethod destroy ((p pane-tasks))
  "Perform actions when destroying <p>."
  (mp:unschedule-timer (timer p)))

