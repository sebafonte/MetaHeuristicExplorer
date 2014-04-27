
(defclass base-model ()
  ())
  

(defmethod initialize-properties :before ((o base-model))
  "Initialize <o> properties."
  nil)

(defmethod initialize-instance :before ((o base-model) &rest keys)
  "Initialize <o>."
  (initialize-properties o)
  (set-default-property-values o))

(defmethod initialize-properties ((o base-model))
  "Initialize <o> properties."
  nil)

(defmethod object ((o base-model))
  o)
  
;;; #TODO: 
;;; Drop modifiers:
;;;
;;;    - shift:   Replace
;;;    - control: Copy / create
;;;    - nada:    Add
;;;
(defmethod drop-action ((o base-model) contenido &optional action-key)
  "Toma la accion conveniente cuando se suelta un objeto sobre el receptor."
  nil)

(defmethod drag-action ((o base-model) &optional action-key)
  "Toma la accion conveniente cuando se dragea sobre el receptor."
  nil)

(defmethod properties-definition ((o base-model))
  "The purpose of this method is to be polimorfic with object-with-properties."
  nil)

(defmethod evolvablep ((o t))
  "Answer whether <o> can be evolved."
  nil)

(defmethod drawablep ((o base-model))
  "Answer whether o can be displayed on the GUI."
  nil)

(defmethod drawablep ((object t))
  "Answer whether o can be displayed on the GUI."
  nil)






