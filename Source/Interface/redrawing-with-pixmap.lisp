(capi:define-interface redrawing-with-pixmap ()
  ((pixmap :initarg :pixmap :initform nil :accessor pixmap)
   (owner :initarg :owner :initform nil :accessor owner))
  (:panes
   (pixmap-object capi:drawn-pinboard-object 
                  :display-callback 'draw-interface-pixmap
                  :accessor pixmap-object))
  (:layouts (pixmap-layout capi:pinboard-layout '((pixmap-object :x 0 :y 0))
                           :pane-menu (lambda (pane object x y)
                                        (make-pane-menu-with-submenus 
                                         pane object x y 
                                         (options-menu-description pane object)))
                           :input-model `(((:button-1 :press)
                                           ,#'(lambda (pane x y)
                                                (drag-example-drag-from pane x y)
                                                (pane-toggle-mouse-cursor-on-drag pane x y)))
                                          ((:button-1 :release)
                                           ,#'(lambda (pane x y)
                                                (drop-example-drop-string-callback pane x y)
                                                (pane-toggle-mouse-cursor-normal pane x y))))
                           ;:drop-callback 'drop-example-drop-string-callback
                           :accessor pixmap-layout))
  (:default-initargs
   :destroy-callback 'destroy-interface-pixmap))


(defmethod (setf pixmap) ((o t) (value t))
  (unless o
    (destroy-interface-pixmap value))
  (setf (slot-value o 'pixmap) o))

(defun destroy-interface-pixmap (interface)
  "Destroys the pixmap of interface."
  (with-slots (pixmap) interface
    (when pixmap
      (gp:destroy-pixmap-port pixmap))))

(defun draw-interface-pixmap (pinboard pinboard-object x y interface-width interface-height)
  "Draws <object> in the pixmap of <pinboard> interface."
  (declare (ignore interface-width)
           (ignore interface-height))
  (let* ((parent-pinboard (capi:element-interface pinboard))
         (pane (pane (owner parent-pinboard)))
         (drawable (drawable-object pane)))
    (draw-in-pixmap pinboard pinboard-object pane drawable parent-pinboard x y)))

(defmethod draw-in-pixmap (pinboard pinboard-object pane drawable parent-pinboard x y)
  "Draws <drawable> in the pixmap of <pinboard> interface."
  (with-slots (pixmap) (capi:element-interface pinboard)
    (let ((width (capi:simple-pane-visible-width parent-pinboard))
          (height (capi:simple-pane-visible-height parent-pinboard)))
      (when (and (drawablep drawable) drawable)
        (capi:with-geometry pinboard-object
          (unless pixmap
            (setf pixmap (create-interface-pixmap pinboard width height)))
          (compute-interface-pixmap pixmap pinboard pane drawable width height)
          (gp:pixblt pinboard boole-1 pixmap x y width height (- x capi:%x%) (- y capi:%y%))
          (post-process-interface-pixmap drawable pixmap width height))))))

(defmethod post-process-interface-pixmap (object pixmap width height)
  "Draw final details of <object> on <pixmap>."
  nil)

(defmethod post-process-interface-pixmap ((object object-in-search) pixmap width height)
  "Draw final details of <object> on <pixmap>."
  (post-process-interface-pixmap-subtask (object object) (context object) pixmap width height))

(defmethod post-process-interface-pixmap-subtask (object subtask pixmap width height)
  "Draw final details of <object> on <pixmap>."
  nil)

(defmethod create-interface-pixmap ((pinboard t) width height)
  "Creates a pixmap port object for pinboard."
  (gp:create-pixmap-port pinboard width height :clear t :background :white))

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

(defun drag-example-drag-object (pane title &rest drag-args)
  ;(capi:drag-pane-object pane drag-args)
  (progn t))

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