(defclass pane-editor-search-algorithm (base-pane pane-editor)
  (;; Model
   (model :initarg model :initform nil :accessor model)
   ;; Default models
   (default-algorithms :initform :default-algorithms :accessor default-algorithms)
   ;; Algorithm creation blocks (#TODO: Check if this is necessary here)
   (blocks :initarg :blocks :initform nil :accessor blocks)
   (arcs :initarg :arcs :initform nil :accessor arcs)))

(defmethod create-interface ((p pane-editor-search-algorithm))
  (make-instance 'interface-editor-search-algorithm))
   
;(capi:define-interface interface-editor-search-algorithm 
    
(defmethod model ((p pane-editor-search-algorithm) object)
  (setf model object)
  (refresh-pane))

(defmethod refresh-pane ((p pane-editor-search-algorithm))
  nil)
   
(defmethod clean ((p pane-editor-search-algorithm))
  (setf (model p) nil))
