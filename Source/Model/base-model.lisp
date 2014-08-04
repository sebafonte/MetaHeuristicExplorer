
(defclass base-model ()
  ())


(defmethod clos:class-extra-initargs ((o base-model))
  '(:forget-defaults))

(defmethod initialize-instance :before ((o base-model) &rest keys)
  "Initialize <o>."
  (initialize-properties o)
  (unless (find :forget-defaults keys)
    (set-default-property-values o)
    (set-default-dependent-property-values o)))

(defmethod initialize-properties :before ((o base-model))
  "Initialize <o> properties."
  nil)

(defmethod initialize-properties ((o base-model))
  "Initialize <o> properties."
  nil)

(defmethod object ((o base-model))
  o)
  
(defmethod properties-definition ((o base-model))
  "Answer <o> properties definition."
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
