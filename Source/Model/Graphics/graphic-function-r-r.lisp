
(defclass graphic-function-r-r (graphic)
  ((xmin :initarg :xmin :accessor xmin)
   (xmax :initarg :xmax :accessor xmax)
   (ymin :initarg :ymin :accessor ymin)
   (ymax :initarg :ymax :accessor ymax)
   (property :initarg :property :accessor property)
   (valuable-x-list :initarg :valuable-x-list :accessor valuable-x-list)
   (valuable-y-list :initarg :valuable-y-list :accessor valuable-y-list)))


;; #TODO: Delete this two functions
(defmethod valuable-x ((g graphic-function-r-r))
  (eval (valuable-x-list g)))

(defmethod valuable-y ((g graphic-function-r-r))
  (eval (valuable-y-list g)))

(defmethod draw-data ((g graphic-function-r-r) heigth width factor-x factor-y xmin ymin)
  "Answer data to draw <g>."
  (let* ((object (subject g))
         (points)
         (valuable-x (valuable-x g))
         (valuable-y (valuable-y g))
         (data (funcall (datasource g) object)))
    (dolist (i data)
      (push (list (* factor-x (- (funcall valuable-x i) xmin))
                  (- heigth (* factor-y (- (funcall valuable-y i) ymin))))
            points))
    points))

(defmethod draw-paired-points (pane point-list)
  "Draws points from point-list on <pane>."
  (do ((i point-list (cdr i)))
      ((or (null i) (null (caddr i))))
    (gp:draw-lines pane 
                   (list (caar i) (cadar i) (caar (cdr i)) (cadar (cdr i)))
                   :foreground (color:make-rgb 1.0 1.0 1.0))))

(defmethod initialize-properties :after ((g graphic-function-r-r))
  "Initialize <g> properties."
  (add-properties-from-values
   g
   (:name 'xmin :label "X min" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :object-parameter t)
   (:name 'xmax :label "X max" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :object-parameter t)
   (:name 'ymin :label "Y min" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :object-parameter t)
   (:name 'ymax :label "Y max" :accessor-type 'accessor-accessor-type 
    :data-type 'number :editor 'number-editor :object-parameter t)
   (:name 'property :label "Property" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'symbol-editor)
   (:name 'valuable-x-list :label "Valuable X" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor)
   (:name 'valuable-y-list :label "Valuable Y" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'lisp-editor)
   (:name 'datasource-list :label "Datasource" :accessor-type 'property-accessor-type 
    :data-type 'list :editor 'lisp-editor 
    :default-value '(lambda (o) (log-data-for-criteria (log-data o) :best-individual)))))

(defmethod compute-interface-pixmap ((pixmap t) (pinboard t) (pane t) (o graphic-function-r-r) width height)
  "Compute pixmap values into <pixmap> of <o>."
  ;; #TODO: Ugly haaackkk!!! delete this shit and modify model to avoid this situtation and pane var !!
  (setf (subject o) (model pane))
  (let* ((compiled-valuable (compiled-valuable o))
         (xmin (xmin o))
         (xmax (xmax o))
         (ymin (ymin o))
         (ymax (ymax o))
         (factor-x (/ width (- (xmax o) (xmin o))))
         (factor-y (/ height (- (ymax o) (ymin o))))
         (x 0) 
         (y 0)
         (points (draw-data o height width factor-x factor-y xmin ymin)))
    (declare (special x) (special y))
    (gp:draw-rectangle pixmap x y width height :filled t :foreground :black)
    (gp:draw-string pixmap (format nil "~A" xmin) 5 (/ height 2) :foreground :red)
    (gp:draw-string pixmap (format nil "~A" xmax) (- width 27) (/ height 2) :foreground :red)
    (gp:draw-string pixmap (format nil "~A" ymin) 5 (- height 5) :foreground :red)
    (gp:draw-string pixmap (format nil "~A" ymax) 5 10 :foreground :red)
    (draw-paired-points pixmap points)))

(defmethod compiled-valuable ((o graphic-function-r-r))
  "Answer a compiled function to evaluate <o>"
  (compile nil (valuable-y-list o)))
 