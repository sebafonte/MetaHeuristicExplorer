
(defclass task-creator-model (base-model)
  ((path :initarg :path :initform nil :accessor path)
   (task :initarg :task :initform nil :accessor task)
   (variations :initarg :variations :initform nil :accessor variations)))


(defmethod add-variation ((o task-creator-model) v)
  (appendf (variations o) (list v)))

(defmethod del-variation ((o task-creator-model) v)
  (removef (variations o) v))

(defmethod edit-variation ((o task-creator-model) source target)
  (setf (nth (position source (variations o)) (variations o)) target))


(defclass pane-task-creator (base-pane)
  ((task :initarg :task :initform nil :accessor task)
   (model :initarg :model :initform (make-instance 'task-creator-model) :accessor model)))


(defmethod initialize-instance :after ((p pane-task-creator) &key task &allow-other-keys)
  (setf (task (model p)) task)
  (update-creator-task (interface p)))

(defmethod interface-class ((p pane-task-creator))
  "Answer <p> interface class."
  'interface-pane-task-creator)


(capi:define-interface interface-pane-task-creator (base-interface)
  ()
  (:panes
   ;; Task
   (label-task capi:title-pane :text "Model")
   (text-task capi:title-pane :text "" :accessor text-task)
   ;; Path
   (label-path capi:title-pane :text "Path")
   (text-path capi:title-pane :text "" :accessor text-path)
   (button-path capi:push-button :text "" :selection-callback 'prompt-tasks-path)
   ;; Variations
   (variations capi:multi-column-list-panel
               :items nil
               :action-callback 'edit-variation-callback 
               :visible-min-height 70
               :accessor variations
               :keep-selection-p t
               :column-function 'get-variations-info
               :pane-menu (lambda (pane object x y)
                            (make-pane-menu 
                             pane 
                             object 
                             x 
                             y 
                             (options-menu-description-task-creator))))
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list 
            (make-instance 'capi:toolbar-button :image 2 :selected-image 2
                           :help-key "Add variation"
                           :selection-callback 'add-variation-callback
                           :callback-type :interface-data)
            (make-instance 'capi:toolbar-button :image 1 :selected-image 1
                           :help-key "Delete variation"
                           :selection-callback 'delete-variation-callback
                           :callback-type :interface-data)
            (make-instance 'capi:toolbar-button :image 3 :selected-image 3
                           :help-key "Create tasks"
                           :selection-callback 'create-variations-callback
                           :callback-type :interface-data))
           :selection nil))
    :callback-type :data-interface
    :title ""
    :title-position :frame
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 8)
    :accessor toolbar))
  (:layouts
   (main-layout capi:column-layout '(layout-task layout-path variations simple-toolbar))
   (layout-task capi:row-layout '(label-task text-task))
   (layout-path capi:row-layout '(label-path button-path text-path)))
  (:default-initargs :best-width 280 :best-height 120 :title "Task creator"))

(defun get-variations-info (&rest args)
  (list args))

(defun options-menu-description-task-creator ()
  '(("Add" add-variation-callback)
    ("Delete" delete-variation-callback)
    ("Edit" edit-variation-selection-callback)))

(defmethod update-interface ((i interface-pane-task-creator))
  "Update <i>."
  (update-creator-task i)
  (update-creator-variations i))

(defun prompt-tasks-path (data interface)
  (declare (ignore data))
  (let ((path (capi:prompt-for-directory "Select result path")))
    (when path
      (setf (path (model (pane interface))) path)
      (update-creator-path interface path)))
  ;(setf (capi:simple-pane-enabled (third (items (toolbar interface)))) 
  ;      (not (null path)))
  )

(defun update-creator-path (interface path)
  (setf (capi:title-pane-text (text-path interface)) 
        (namestring path)))

(defun update-creator-task (interface)
  (let ((task (task (model (pane interface)))))
    (when task
      (setf (capi:title-pane-text (text-task interface)) (name task)))))

(defun update-creator-variations (interface)
  (setf (capi:collection-items (variations interface))
        (variations (model (pane interface)))))

(defun add-variation-callback (interface data)
  (declare (ignore data))
  (let ((p (pane interface)))
    (let ((variation (capi:prompt-for-string
                      "Enter variation" 
                      :text "((algorithm population-size) . (:from 5 :to 500 :step 10))")))
      (when (and variation (valid-variation-p (read-from-string variation)))
        (add-variation (model p) variation)
        (update-creator-variations interface)))))

(defun valid-variation-p (description)
  (not (null (variations-from-description description))))

(defun create-variations-callback (interface data)
  (declare (ignore data))
  (let* ((model (model (pane interface)))
         (creator (make-instance 'task-creator :variations-description (mapcar 'read-from-string (variations model)))))
    (save-task-on creator (create-tasks-on creator (task model)) (path model))))

(defun delete-variation-callback (interface data)
  "Add a new connection configured by the user to the model of <p>."
  (let ((p (pane interface))
        (selection (capi:choice-selected-item (variations interface))))
    (when selection
      (del-variation (model p) selection)
      (update-creator-variations interface))))

(defun edit-variation-callback (data interface)
  "Add a new connection configured by the user to the model of <p>."
  (let ((p (pane interface))
        (variation (capi:prompt-for-string "Edit variation" :text data)))
    (when (and variation (valid-variation-p (read-from-string variation)))
      (edit-variation (model p) data variation)
      (update-creator-variations interface))))

(defun edit-variation-selection-callback (interface data)
  "Add a new connection configured by the user to the model of <p>."
  (edit-variation-callback (capi:choice-selected-item (variations interface)) interface))
