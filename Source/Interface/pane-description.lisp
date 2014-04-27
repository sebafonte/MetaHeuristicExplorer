(defclass pane-description (base-model)
  ((pane :initarg :pane :accessor pane)
   (interface-class :initarg :interface-class :accessor interface-class)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (heigth :initarg :heigth :accessor heigth)
   (width :initarg :width :accessor width)))


(defmethod get-description (pane)
  "Answer a pane-description instance with <pane> description."
  (let ((interface (interface pane)))
    (capi:with-geometry 
        (interface pane)  
      (make-instance 'pane-description 
                     :pane pane
                     :interface-class (class-name (class-of interface))
                     :x (first (capi:interface-geometry interface))
                     :y (second (capi:interface-geometry interface))
                     :heigth capi:%height%
                     :width capi:%width%))))
        
(defmethod new-from-description ((o pane-description))
  "Answer a new instance of the pane described by <o>."
  (let ((pane (pane o)))
    (appendf (interface-arguments pane) 
             (list :x (x o) :y (y o) :best-height (heigth o) :best-width (width o)))
    (initialize-interface pane)
    (open-pane pane :mdi-interface (interface *main-pane*))))

(defmethod post-initialize-interface ((p base-pane))
  "Post initialize actions for <p>."
  nil)


