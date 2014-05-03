;;; Functions for a mini-state machine to handle drag & drop behaviout
;;;		- *drag-context* has the object being dragged
;;;
;;; #TODO: - Take out the last condition, i think it's unecessary now
;;;        - Use ccase
;;; 

(defvar *drag-state* 'IDLE)
(defvar *drag-selected-object*)
(defvar *drag-context* (make-instance 'selection-context))


(defun process-press (self x y)
  (declare (ignore x))
  (declare (ignore y))
  (cond ((equal *drag-state* 'IDLE) (progn 
                                       (save-drag-source-properties self)
                                       (setf *drag-state* 'PRESSED)))
        (t nil)))

(defun process-second-press (self x y)
  (declare (ignore x) (ignore y) (ignore self))
  (cond ((equal *drag-state* 'PRESSED) (progn 
                                          (double-clicked)
                                          (setf *drag-state* 'IDLE)))
        (t nil)))

(defun process-motion (self x y)
  (declare (ignore x) (ignore y) (ignore self))
  (cond ((equal *drag-state* 'PRESSED) (progn 
                                          (relative-motion)
                                          (setf *drag-state* 'DRAGGING)))
        ((equal *drag-state* 'DRAGGING) (progn 
                                           (relative-motion-dragging)
                                           (setf *drag-state* 'DRAGGING)))
        (t nil)))

(defun process-release (self x y)
  (declare (ignore x) (ignore y))
  (cond ((equal *drag-state* 'PRESSED) (progn 
                                          (clicked)
                                          (setf *drag-state* 'IDLE)))
        ((equal *drag-state* 'DRAGGING) (progn 
                                           (finalize-drag self)
                                           (setf *drag-state* 'IDLE)))
        (t nil)))

(defun save-drag-source-properties (pane) 
  (setf *drag-selected-object* (capi:element-parent (capi:element-parent pane)))
  (print "Save mouse position."))

(defun double-clicked () 
  (print "Double click."))

(defun clicked ()
  (print "Clicked."))

(defun relative-motion () 
  (print "Moving."))

(defun relative-motion-dragging () 
  (print "Dragging."))

(defun finalize-drag (pane)
  (if *drag-selected-object*
      (set-model (capi:element-parent (capi:element-parent pane)) 
                 *drag-selected-object*))
  (print "Drag ended."))

(defun pane-toggle-mouse-cursor-normal (pane x y)
  (declare (ignore x y))
  (setf (capi:simple-pane-cursor pane) nil)
  (toggle-mouse-cursor (interface *main-pane*) nil))

(defun pane-toggle-mouse-cursor-on-drag (pane x y)
  (declare (ignore x y))
  (setf (capi:simple-pane-cursor pane) :move)
  (toggle-mouse-cursor (interface *main-pane*) :move))

(defun toggle-mouse-cursor (interface value)
  (capi:apply-in-pane-process
   (interface *main-pane*)
   (lambda ()
     (setf (capi:simple-pane-cursor (interface *main-pane*)) value))))


(defun main-interfaces-list ()
  (let* ((interface (interface *main-pane*))
         (container (capi:document-frame-container interface)))
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

(defmethod drag-example-drag-from ((pane capi:opengl-pane) x y)
  (let ((object (model (pane (capi:element-interface pane)))))
    (typecase object
      ;; Pinboard items (#CHECK)
      (capi:item-pinboard-object
       (let ((string (capi:item-text object)))
         (drag-example-drag-object pane string nil :string string)))
      ;; Graphics
      ;; Search objects
      (object-in-search
       (drag-example-drag-object pane (format nil "DRAG OBJECT") object)))
     nil))

;; #TODO: Remove when refactored
(defmethod drag-example-drag-from ((pane t) x y)
  (let ((object (model (pane (owner (capi:element-parent pane))))))
    (typecase object
      ;; Pinboard items (#CHECK)
      (capi:item-pinboard-object
       (let ((string (capi:item-text object)))
         (drag-example-drag-object pane string nil :string string)))
      ;; Graphics
      ;; Search objects
      (object-in-search
       (drag-example-drag-object pane (format nil "DRAG OBJECT") object)))
     nil))

;; #TODO: Check what is better here
(defun drag-example-drag-object (pane title &rest drag-args)
  ;(capi:drag-pane-object pane drag-args)
  nil)

#|
  ;; #TODO: Remove when refactored
(defmethod drop-example-drop-string-callback ((pane t) x y)
  (let ((target (editor-under-position pane x y))
        (pane-interface (pane (owner (capi:element-interface pane)))))
    (when (and (not (equal target pane-interface)) target)
      (set-model (pane target) (model pane-interface)))))
|#

(defmethod drop-example-drop-string-callback ((pane t) x y)
  (let ((target (editor-under-position pane x y))
        (pane-interface (pane (capi:element-interface pane))))
    (when (and (not (equal target pane-interface)) target)
      (set-model (pane target) (model pane-interface)))))

#|
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