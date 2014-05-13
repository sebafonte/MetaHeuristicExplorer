
(defclass pane-task-creator (base-pane)
  ((model :initarg :model :accessor model)))


(defmethod interface-class ((p pane-task-creator))
  "Answer <p> interface class."
  'interface-pane-task-creator)


(capi:define-interface interface-pane-task-creator (base-interface)
  ()
  (:panes
   (label-task capi:title-pane :text "Model")
   (pane-task capi:title-pane :text "Task")
   (variations capi:multi-column-list-panel
               :items nil
               :action-callback 'open-properties-callback 
               :visible-min-height 70
               :accessor connections
               :keep-selection-p t
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
           (list (make-instance 'capi:toolbar-button :image 2 :selected-image 2
                                :help-key "Create tasks"
                                :selection-callback 'task-creator-create-callback
                                :callback-type :interface-data))
           :selection nil))
    :callback-type :data-interface
    :title ""
    :title-position :frame
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 8)))
  (:layouts
   (main-layout capi:column-layout '(layout-task variations simple-toolbar))
   (layout-task capi:row-layout '(label-task pane-task))
   (buttons capi:row-layout '(button-add button-add button-disconnect button-delete button-scan button-test)))
  (:default-initargs :best-width 200 :best-height 150 :title "Task creator"))

(defun options-menu-description-task-creator ()
  '(("Add property" add-variation-callback)
    ("Delete property" delete-variation-callback)
    ("Edit property" edit-variation-callback)))

(defmethod initialize-instance :after ((i interface-pane-task-creator) &rest args)
  nil)

(defmethod update-interface ((i interface-pane-task-creator))
  "Update <i>."
  (setf (capi:collection-items (variations i))
        (variations (connection-administrator (model (pane i))))))

(defun add-variation-callback (interface data)
  "Add a new connection configured by the user to the model of <p>."
  (declare (ignore data))
  (let ((p (pane interface)))
    (when variation
      (add-variation (model p) variation)
      (update-interface interface))))

(defun delete-variation-callback (interface data)
  "Add a new connection configured by the user to the model of <p>."
  (let ((p (pane interface)))
    (when variation
      (add-variation (model p) variation)
      (update-interface interface))))

(defun edit-variation-callback (interface data)
  "Add a new connection configured by the user to the model of <p>."
  nil)

(defun prompt-tasks-path (interface data)
  "Open a dialog to save <pane> to a specified file."
  (let ((file (pane-explorer-prompt-file pane :save)))
    (when file
      (if (probe-file file) (delete-file file))
      (remap-population-interface (interface pane)))))

#|
(capi:prompt-for-directory 
   "Select result path"
   ;:filter "*.crossover-pane"
   :operation :save
   ;:filters `("Crossover pane files" "*.crossover-pane")
   )
|#