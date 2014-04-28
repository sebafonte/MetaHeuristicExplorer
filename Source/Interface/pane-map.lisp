
(defclass pane-map (base-pane object-with-properties)
  ((graph :initarg :graph :accessor graph)
   (distribution-mode :initarg :distribution-mode :accessor distribution-mode)
   (auto-generate :initarg :auto-generate :initform nil :accessor auto-generate)
   (algorithm :initarg :algorithm :accessor algorithm)))


(defmethod interface-class ((p pane-map))
  "Answer <p> interface class."
  'interface-pane-map)

(capi::define-interface interface-pane-map (base-interface)
  ()
  (:panes 
   (distribution-mode
    capi:option-pane
    :accessor distribution-mode
    :items '(root-parent-relation-map)
    :title "Distribution mode"
    :callback-type :interface-data
    :selection-callback 'refescar-mapa)
   (graph capi:tree-view 
          :roots '(1) :children-function '1+
          :pane-menu (lambda (pane object x y)
                       (make-pane-menu pane object x y (pane-map-node-description-menu)))))
  (:layouts (main capi:column-layout '(distribution-mode graph)))
  (:default-initargs :best-width 400 :best-height 400 :title "Objects map"))
 
(defmethod pane-map-node-description-menu ()
  "Answer menu description for a selected object on pane-map."
  '())

(defmethod generate-map ((p pane-map) generation-function)
  "Generate <p> model."
  nil)

(defmethod add-object ((p pane-map) object)
  "Add <object> to <p> map."
  nil)

(defmethod delete-arc ((p pane-map) a b)
  "Delete the arc going from <a> to <b> from <p> map."
  nil)

(defmethod delete-object ((p pane-map) object)
  "Delete <object> from <p> map."
  nil)

(defmethod clear ((p pane-map))
  "Clear <p>."
  (setf (graph p) nil))
