
(defclass pane-editor-lisp (capi:editor-pane) ()
  (:default-initargs
   :buffer-modes '("Lisp")
   :echo-area t
   :text ";; Editor"
   :name "Expression editor"))


(capi:define-interface interface-pane-editor-lisp ()
  ()
  (:panes (editor pane-editor-lisp))
  (:default-initargs))