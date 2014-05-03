
(capi::define-interface interface-pane-editor-entity-base (base-interface)
  ())


(defmethod set-model :after ((i interface-pane-editor-entity-base) (object t))
  "Set <object> as <i> model."
  (declare (ignore object))
  (trigger i :model-changed i))

(defmethod initialize-instance :after ((i interface-pane-editor-entity-base) &rest keys)
  (initialize-viewer i)
  (appendf *interface-editors* (list i)))

(defmethod initialize-viewer ((i interface-pane-editor-entity-base))
  (setf (owner (pane-image i)) i))

(defmethod destroy-interface :before ((i interface-pane-editor-entity-base))
  "Perform actions when <interface> is destroyed."
  (setf *interface-editors* (reject *interface-editors* (lambda (object) (equal object i)))))

(defmethod destroy-interface ((i interface-pane-editor-entity-base))
  "Perform actions when <interface> is destroyed."
  (destroy-interface-pixmap i))

(defmethod set-editor-tab-to ((i interface-pane-editor-entity-base) name)
  "Set tab with <name> on <i>."
  (capi:execute-with-interface 
   i
   (lambda (&rest args)
     (declare (ignore args))
     (setf (capi:choice-selection (tab i)) (editor-index i name)))))

(defmethod pane-entity-copy ((i interface-pane-editor-entity-base) &optional operation)
  "Add <i> model to *drag-context* clipboard context."
  (declare (ignore operation))
  (add-to-context *drag-context* :object (model (pane (element-interface i)))))

(defmethod pane-entity-import ((i interface-pane-editor-entity-base) data)
  "Import <i> model from a file, prompting user for location."
  (declare (ignore data))
  (let ((path (capi:prompt-for-file "Select file to import"
                                    :filter "*.explorer"
                                    :operation :open
                                    :filters `("Explorer files" "*.explorer"))))
    (if path 
        (set-model 
         (pane i) 
         (eval (read-from-string (car (load-from-file path :tag "#source-description"))))))))

(defmethod pane-entity-export ((i interface-pane-editor-entity-base) data)
  "Export <i> model to a file, prompting user for location."
  (declare (ignore data))
  (let ((pane (pane (element-interface i))))
    (if (model pane)
        (let ((path (capi:prompt-for-file "Select file to export"
                                          :filter "*.explorer"
                                          :operation :save
                                          :filters `("Explorer files" "*.explorer"))))
          (when path
            (if (probe-file path) (delete-file path))
            (if path (save-source-description (model pane) path)))))))

(defmethod pane-entity-editor-edit-properties ((i interface-pane-editor-entity-base) data)
  (open-in-new-editor (pane (element-interface i)) i))

(defmethod pane-entity-send ((i interface-pane-editor-entity-base) data)
  "Send selected object on <i> to a selected target."
  (let ((descriptors (prompt-for-default-object-choices
                     "Select target host"
                     (connections (connection-administrator (system-get 'local-distributed-environment))))))
    (dolist (descriptor descriptors)
      (send-object descriptor (model (pane (element-interface i)))))))

(defmethod pane-entity-set-new-model ((i interface-pane-editor-entity-base) data)
  "Set a new default model on <i>."
  (set-new-model (pane i)))

(defmethod pane-entity-save-new-model ((i interface-pane-editor-entity-base) data)
  "Save <i> pane model."
  (save-new-model (pane i)))

(defmethod pane-entity-inyect-expression ((i interface-pane-editor-entity-base) data)
  "Replaces subtree at selected node on <i> with an user input prompted expression."
  (declare (ignore data))
  (let ((index-subtree (capi:choice-selection (pane-graph i)))
        (object (model (pane i))))
    (multiple-value-bind (accepted-p input) 
        (prompt-for-sub-expression "Ingrese una expresión")
      (when accepted-p
        (setf (program object) 
              (replace-internal-subtree 
               (program object)
               input
               index-subtree
               (language (algorithm (context object)))))
        (set-model i object)))))

(defmethod pane-entity-view-introns (interface data)
  "Shows introns on interface graph pane."
  (let* ((object (model (pane interface)))
         (editor (make-instance 'pane-editor-entity 
                                :model object 
                                :mdi-interface interface
                                :interface-mode 'interface-pane-editor-entity-wiggy
                                :edges-weight-values (introns-detection object))))
    (set-model editor object)
    (open-pane editor)))

(defmethod set-title ((i interface-pane-editor-entity-base) object)
  "Set <i> title to <object>."
  (setf (capi:interface-title i) 
        (format nil "Editor on ~S" (class-name (class-of object)))))

(defmethod set-graph-model ((i interface-pane-editor-entity-base) object)
  "Set <i> graph model to <object>."
  (when (pane-graph i)
    (capi:apply-in-pane-process
     (pane-graph i)
     #'(setf capi:graph-pane-roots)
     (list "" (genotype-nodes-data object))
     (pane-graph i))))

(defmethod set-editor-model ((i interface-pane-editor-entity-base) object)
  "Set <i> text editor model to <object>."
  (when (pane-editor i)
    (let ((stream-format (make-string-output-stream))
          (*print-right-margin* 25)
          (*print-lines* 1000)
          (program (if (and object (evolvablep object) (object object))
                       (program object))))
      (pprint program stream-format)
      (capi:apply-in-pane-process
       (pane-editor i)
       #'(setf capi:editor-pane-text)
       (get-output-stream-string stream-format)
       (pane-editor i)))))

(defmethod set-image-model ((i interface-pane-editor-entity-base) object image-editor)
  "Set <i> image model to <object>."
  (declare (ignore image-editor))
  (when (pane-image i)
    (setf (pixmap (pane-image i)) nil)
    (when (capi:pane-layout (pane-image i))
      (capi:execute-with-interface 
       i
       (lambda (&rest args)
         (capi:redraw-pinboard-layout (capi:pane-layout (pane-image i)) 0 0
                                      (capi:simple-pane-visible-width i)
                                      (capi:simple-pane-visible-height i)))))))
