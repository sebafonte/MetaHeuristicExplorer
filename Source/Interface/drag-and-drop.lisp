
(defvar *drag-context* (make-instance 'selection-context))


(defun pane-toggle-mouse-cursor-normal (pane x y)
  (declare (ignore x y))
  (setf (capi:simple-pane-cursor pane) nil)
  (toggle-mouse-cursor (interface *main-pane*) nil))

(defun pane-toggle-mouse-cursor-on-drag (pane x y)
  (declare (ignore x y))
  (setf (capi:simple-pane-cursor pane) :move)
  (toggle-mouse-cursor (interface *main-pane*) :move))

(defun toggle-mouse-cursor (interface value)
  (declare (ignore interface))
  (capi:apply-in-pane-process
   (interface *main-pane*)
   (lambda ()
     (setf (capi:simple-pane-cursor (interface *main-pane*)) value))))

(defun main-interfaces-list ()
  (let ((interface (interface *main-pane*)))
    (capi:collect-interfaces 'base-interface :screen interface :sort-by :visible)))


;;; #TODO: Try to use
;;;	- Native drag & drop from Lispworks
;;;     - (capi::pinboard-object-at-position pinboard-layout-target new-x new-y)
;;;     - examples/capi/elements/convert-relativeposition.lisp
;;;
(defmethod editor-under-position ((interface t) x y)
  "Answer the editor under <x> and <y> mouse position relative to <interface>."
  (let ((editors)
        (pinboard-layout-source interface)
        (interfaces (main-interfaces-list)))
    (dolist (editor *interface-editors*)
      (let ((pinboard-layout-target (graphic-part editor)))
        (if (capi:element-interface pinboard-layout-target)
            (multiple-value-bind (new-x new-y)
                (capi::convert-relative-position pinboard-layout-source pinboard-layout-target x y)
              (capi:with-geometry pinboard-layout-target
                (if (and (<= new-x capi:%width%)
                         (>= new-x 0)
                         (<= new-y capi:%height%)
                         (>= new-y 0))
                    (appendf editors (list (list editor (capi:top-level-interface editor))))))))))
    (caaar (sort (mapcar (lambda (item) 
                           (list item (position (second item) interfaces)))
                         editors)
                 (lambda (x y) 
                   (< (cadr x) (cadr y)))))))

(defmethod drag-from-pane ((pane capi:opengl-pane) x y)
  "Perform actions when dropping an object dragged from <pane>."
  (let ((object (model (pane (capi:element-interface pane)))))
    (typecase object
      ;; Pinboard items (#CHECK)
      (capi:item-pinboard-object
       (let ((string (capi:item-text object)))
         (drop-from-pane-object pane string nil :string string)))
      (object-in-search
       (drop-from-pane-object pane (format nil "DRAG OBJECT") object)))
     nil))

(defmethod drop-from-pane ((pane t) x y)
  "Perform actions when dropping an object dragged from <pane>."
  (let ((target (editor-under-position pane x y))
        (pane-interface (pane (capi:element-interface pane))))
    (when (and (not (equal target pane-interface)) target)
      (set-model (pane target) (model pane-interface))
      (trigger target :interface-model-changed target))))

(defun drop-from-pane-object (pane title &rest drag-args)
  "Perform actions when dragging an object from <pane>."
  (declare (ignore pane title drag-args))
  nil)

#|
;; #TODO: Remove when refactored
(defmethod drag-from-pane ((pane t) x y)
  (let ((object (model (pane (owner (capi:element-parent pane))))))
    (typecase object
      ;; Pinboard items (#CHECK)
      (capi:item-pinboard-object
       (let ((string (capi:item-text object)))
         (drop-from-pane-object pane string nil :string string)))
      (object-in-search
       (drop-from-pane-object pane (format nil "DRAG OBJECT") object)))
     nil))

  ;; #TODO: Remove when refactored
(defmethod drop-from-pane ((pane t) x y)
  (let ((target (editor-under-position pane x y))
        (pane-interface (pane (owner (capi:element-interface pane)))))
    (when (and (not (equal target pane-interface)) target)
      (set-model (pane target) (model pane-interface))
      (trigger target :interface-model-changed target))))


;;; #TODO: 
;;; Drop modifiers:
;;;
;;;    - none: 		Add
;;;    - shift:   	Replace
;;;    - control: 	Copy / create
;;;

(defmethod drop-action ((pane pane-buffer) (o entity) &optional action-key)
  "Perform drop actions when dropping <o> on <pane>."
  nil)

(defmethod drop-action ((pane pane-buffer) (p population) &optional action-key)
  "Perform drop actions when dropping <p> on <pane>."
  nil)
|#