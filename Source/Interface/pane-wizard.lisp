;; Default system usages configured in examples.
;;

(defclass pane-wizard (base-pane)
  ((items :initarg :items :initform nil :accessor items)))

(capi:define-interface interface-pane-wizard ()
  ()
  (:panes 
   (button-open capi:push-button :text "Open wizard"))

  (:layouts
   (main-layout capi:column-layout '(label-tittle items buttons))
   (buttons capi:row-layout '(button-open))
   )
  )
