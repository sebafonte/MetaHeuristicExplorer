;;; Language for expressions which repressent a free OpenGL expressions
;;;
;;;  - 2d drawing set
;;;  - points, lines, triangle, quads, polygon & strips
;;;  - push, pop, load-identity matrix operations
;;;  - color, texture
;;;  - translate, scale, rotate
;;;  - +, -, *, /-, abs, cos, sin functions
;;;  - repeat-n times control function
;;;  - repeat-var function to retrieve loop index
;;;

;; Max loop iterations parameter
(defparameter *gl-app-max-iterations* 5000)

;; OpenGL free drawing language definition 
(defparameter *opengl-free-drawing-expression-tokens-2d*
  '(;; Keywords
    (gl-app-draw :draw)
    ;; Low level drawing
    (gl-app-begin-points-2d :gl-app-point)
    (gl-app-begin-lines-2d :gl-app-line)
    (gl-app-begin-triangles-2d :gl-app-triangle)
    (gl-app-begin-triangles-strip-2d :gl-app-triangle-strip)
    (gl-app-begin-quads-2d :gl-app-quad)
    (gl-app-begin-quads-strip-2d :gl-app-quad-strip)
    (gl-app-begin-polygon-2d :gl-app-polygon)
    ;; OpenGL specific
    (gl-app-push-matrix :push-matrix)
    (gl-app-pop-matrix :pop-matrix)
    (gl-app-load-identity :load-identity)
    ;; High level drawing
    (gl-app-point-2d :2-ary-high-primitive-operator)
    (gl-app-line-2d :4-ary-high-primitive-operator)
    (gl-app-triangle-2d  :6-ary-high-primitive-operator)
    (gl-app-quad-2d :8-ary-high-primitive-operator)
    ;(gl-app-cube-2d :1-ary-high-primitive-operator)
    ;(gl-app-sphere-2d :3-ary-high-primitive-operator)
    ;(gl-app-circle-2d :3-ary-high-primitive-operator)
    ;(gl-app-cylinder-2d :5-ary-high-primitive-operator)
    ;(gl-app-prog-high-level-primitive :2-ary-high-primitive-operator)
    ;; Graphic state modifiers
    (gl-app-point-size :1-ary-high-primitive-operator)
    (gl-app-line-width :1-ary-high-primitive-operator)
    ;; Vertex operations
    ;(gl-app-tex-coord-2d :2-ary-vertex-operator)
    (gl-app-vertex-2d :2-ary-vertex-operator)
    (gl-app-color :3-ary-vertex-operator)
    (gl-app-rotate-2d :1-ary-vertex-operator)
    (gl-app-translate-2d :2-ary-vertex-operator)
    (gl-app-scale-2d :2-ary-vertex-operator)
    ;(gl-app-prog-vertex :2-ary-vertex-operator)
    ;; Expression to vertex conversion
    (gl-app-cvec2 :cvec2)
    ;; Control
    (gl-app-repeat-n :2-ary-control-operator)
    (gl-app-grid-repeat :4-ary-high-primitive-operator)
    (gl-app-set-var :set-var)
    ;; Functions
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (asin :1-ary-operator)
    (cos :1-ary-operator)
    ;(tan :1-ary-operator)
    ;(atan :1-ary-operator)
    ;(sqr :1-ary-operator)
    ;; Operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/- :2-ary-operator)
    ;; Variables
    (x :var)
    (y :var)
    (z :var)
    ;; Time variable
    (*time-variable* :time-var)
    ;; Iteration vars
    (i :iteration-var)
    (j :iteration-var)
    (k :iteration-var)))

(defparameter *gl-app-max-iterations* 2000)

(defun initialize-opengl-free-drawing-expression-parser-2d (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start start-draw)
                $1)
               ((start-draw :open :draw draw-expression-list :close)
                `(:start-draw (,$2 ,$3)))
               ;; Draw operation list
               ((draw-expression-list draw-operation)
                `(:draw-expression-list ,$1))
               ((draw-expression-list draw-operation draw-expression-list)
                `(:draw-expression-list ,$1 ,$2))
               ;; Draw operations: opengl matrix
               ((draw-operation :open :load-identity :close)
                `(:draw-operation (,$2)))
               ((draw-operation :open :push-matrix :close)
                `(:draw-operation (,$2)))
               ((draw-operation :open :pop-matrix :close)
                `(:draw-operation (,$2)))
               ;; Draw operations: primitives
               ((draw-operation 
                 :open :1-ary-high-primitive-operator expression :close)
                `(:draw-operation (,$2 ,$3)))
               ((draw-operation 
                 :open :2-ary-high-primitive-operator expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4)))
               ((draw-operation 
                 :open :3-ary-high-primitive-operator expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5)))
               ((draw-operation 
                 :open :4-ary-high-primitive-operator expression expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5 ,$6)))
               ((draw-operation 
                 :open :5-ary-high-primitive-operator 
                 expression expression expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5 ,$6 ,$7)))
               ((draw-operation 
                 :open :6-ary-high-primitive-operator 
                 expression expression expression expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8)))
               ((draw-operation 
                 :open :7-ary-high-primitive-operator 
                 expression expression expression expression expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9)))
               ((draw-operation
                 :open :8-ary-high-primitive-operator 
                 expression expression expression expression expression expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 ,$10)))
               ((draw-operation 
                 :open :set-var :var expression :close)
                `(:draw-operation (,$2 ,$3 ,$4)))
               ;; Draw operations: vertex operations
               ((draw-operation :open :1-ary-vertex-operator expression :close)
                `(:draw-operation (,$2 ,$3)))
               ((draw-operation :open :2-ary-vertex-operator expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4)))
               ((draw-operation :open :3-ary-vertex-operator expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5)))
               ((draw-operation :open :4-ary-vertex-operator expression expression expression expression :close)
                `(:draw-operation (,$2 ,$3 ,$4 ,$5 ,$6)))
               ;; Control operations
               ((draw-operation :open :2-ary-control-operator expression draw-expression-list :close)
                `(:draw-operation (,$2 ,$3 ,$4)))
               ;; Expression
               ((expression :open :1-ary-operator expression :close)
                `(:expression (,$2 ,$3)))
               ((expression :open :2-ary-operator expression expression :close)
                `(:expression (,$2 ,$3 ,$4)))
               ((expression :open :3-ary-operator expression expression expression :close)
                `(:expression (,$2 ,$3 ,$4 ,$5)))
               ((expression :open :4-ary-operator expression expression expression expression :close)
                `(:expression (,$2 ,$3 ,$4 ,$5 ,$6)))
               ;; Terminals
               ((expression :var)
                `(:expression ,$1))
               ((expression :time-var)
                `(:expression ,$1))
               ((expression :iteration-var)
                `(:expression ,$1))
               ((expression :constant)
                `(:expression ,$1)))))

#|
	;; Vertex expression: #NOTE: To add points expressions
    ((vertex-expression :open :cvec2 expression expression :close)
		`(:vertex-expression ,$2 ,$3 ,$4))
|#

(defun opengl-free-drawing-grammar-productions ()
  '((start start-draw)
    (start-draw :open :draw draw-expression-list :close)
    ;; Draw operation list
    (draw-expression-list draw-operation)
    (draw-expression-list draw-operation draw-expression-list)
    ;; Draw operations: opengl matrix
    (draw-operation :open :load-identity :close)
    (draw-operation :open :push-matrix :close)
    (draw-operation :open :pop-matrix :close)
    ;; Draw operations: primitives
    (draw-operation :open :1-ary-high-primitive-operator expression :close)
    (draw-operation :open :2-ary-high-primitive-operator expression expression :close)
    (draw-operation :open :set-var :var expression :close)
    ;(draw-operation :open :3-ary-high-primitive-operator expression expression expression :close)
    ;(draw-operation :open :4-ary-high-primitive-operator vertex-expression vertex-expression vertex-expression 
    ;                vertex-expression :close)
    ;(draw-operation :open :5-ary-high-primitive-operator 
    ;                expression expression expression expression expression :close)
    (draw-operation :open :6-ary-high-primitive-operator 
                    expression expression expression expression expression expression :close)
    ;(draw-operation :open :7-ary-high-primitive-operator 
    ;                expression expression expression expression expression expression expression :close)
    (draw-operation :open :8-ary-high-primitive-operator 
                    expression expression expression expression expression expression expression expression :close)
    ;; Vertex operators
    ;(draw-operation :open :1-ary-vertex-operator expression :close)
    ;(draw-operation :open :2-ary-vertex-operator expression expression :close)
    ;(draw-operation :open :3-ary-vertex-operator expression expression expression :close)
    ;(draw-operation :open :4-ary-vertex-operator expression expression expression expression :close)
    ;; Control operations
    (draw-operation :open :2-ary-control-operator expression draw-expression-list :close)
    ;; Expression
    (expression :open :1-ary-operator expression :close)
    (expression :open :2-ary-operator expression expression :close)
    ;(expression :open :3-ary-operator expression expression expression :close)
    ;(expression :open :4-ary-operator expression expression expression expression :close)
    ;; Terminals
    (expression :var)
    ;(expression :time-var)
    ;(expression :iteration-var)
    (expression :constant)))

(defun entity-opengl-free-drawing-default-functions-info-2d ()
  '((gl-app-draw 2)
    ;; OpenGL specific
    (gl-app-push-matrix 0)
    (gl-app-pop-matrix 0)
    (gl-app-load-identity 0)
    ;; High level drawing
    (gl-app-point-2d 2)
    (gl-app-line-2d 4)
    (gl-app-triangle-2d  6)
    (gl-app-quad-2d 8)
    ;(gl-app-cube-2d 1)
    ;(gl-app-sphere-2d 3)
    ;(gl-app-circle-2d 3)
    ;(gl-app-cylinder-2d 5)
    ;(gl-app-prog-high-level-primitive 2)
    ;; Graphic state modifiers
    (gl-app-point-size 1)
    (gl-app-line-width 1)
    ;; Vertex operations
    (gl-app-color 3)
    (gl-app-vertex-2d 2)
    (gl-app-rotate-2d 1)
    (gl-app-translate-2d 2)
    (gl-app-scale-2d 2)
    ;(gl-app-tex-coord-2d 2)
    ;; Expression to vertex conversion
    (gl-app-cvec2 2)
    ;; Control
    (gl-app-repeat-n 2)
    (gl-app-grid-repeat 4)
    (gl-app-set-var 2)
    ;(gl-app-prog-vertex 2)
    ;; Math function:2-ary-control-operators
    (abs 1)
    (sin 1)
    (asin 1)
    (cos 1)
    ;(tan 1)
    ;(atan 1)
    ;(sqr 1)
    (+ 2)
    (- 2)
    (* 2)
    (/- 2)))


(defparameter *opengl-free-drawing-editing-patterns-2d*
  nil)

;; OpenGL Free Drawing specific functions
;; Keywords
(defun gl-app-draw (&rest args)
  nil)

;; Basic figures
(defun gl-app-begin-points-2d (&rest args)
  nil)

(defmacro gl-app-begin-lines-2d (&rest args)
  `(progn 
     (opengl:gl-begin opengl:*gl-lines*)
      ,args
     (opengl:gl-end)))

(defun gl-app-begin-triangles-2d (&rest args)
  nil)

(defun gl-app-begin-triangles-strip-2d (&rest args)
  nil)

(defun gl-app-begin-quads-2d (&rest args)
  nil)

(defun gl-app-begin-quads-strip-2d (&rest args)
  nil)

(defun gl-app-begin-polygon-2d (&rest args)
  nil)

(defun gl-app-vertex-2d (x y)
  (opengl:gl-vertex2-f (safe-float-coerce x) (safe-float-coerce y)))

#|
(defun gl-app-tex-coord-2d (r s)
  (opengl:gl-tex-coord-2f (safe-float-coerce r) (safe-float-coerce s)))

;; High level figures
(defun gl-app-cube (a)
  nil)

(defun gl-app-sphere-2d (radius slices stacks)
  (let ((quadric))
    (opengl:glu-sphere quadric radius slices stacks)))

(defun gl-app-circle-2d (inner-radius outer-radius slices loops)
 (let ((quadric))
   (opengl:glu-cylinder quadric inner-radius outer-radius slices loops)))

(defun gl-app-cylinder-2d (base-radius top-radius height slices stacks)
  (let ((quadric (opengl:glu-quadric-obj ""))
    (opengl:glu-cylinder 
     quadric 
     (safe-float-coerce base-radius) 
     (safe-float-coerce top-radius)  
     (safe-float-coerce height)
     (safe-float-coerce slices)
     (safe-float-coerce stacks))))
|#

(defun gl-app-point-2d (x y)
  (opengl:gl-begin opengl:*gl-points*)
  (opengl:gl-vertex2-f (safe-float-coerce x) (safe-float-coerce y))
  (opengl:gl-end))

(defun gl-app-line-2d (a b c d)
  (opengl:gl-begin opengl:*gl-lines*)
  (opengl:gl-vertex2-f (safe-float-coerce a) (safe-float-coerce b))
  (opengl:gl-vertex2-f (safe-float-coerce c) (safe-float-coerce d))
  (opengl:gl-end))

(defun gl-app-triangle-2d (a b c d e f)
  (opengl:gl-begin opengl:*gl-triangles*)
  (opengl:gl-vertex2-f (safe-float-coerce a) (safe-float-coerce b))
  (opengl:gl-vertex2-f (safe-float-coerce c) (safe-float-coerce d))
  (opengl:gl-vertex2-f (safe-float-coerce e) (safe-float-coerce f))
  (opengl:gl-end))

(defun gl-app-quad-2d (a b c d e f g h)
  (opengl:gl-begin opengl:*gl-quads*)
  (opengl:gl-vertex2-f (safe-float-coerce a) (safe-float-coerce b))
  (opengl:gl-vertex2-f (safe-float-coerce c) (safe-float-coerce d))
  (opengl:gl-vertex2-f (safe-float-coerce e) (safe-float-coerce f))
  (opengl:gl-vertex2-f (safe-float-coerce g) (safe-float-coerce h))
  (opengl:gl-end))

(defun gl-app-prog-vertex (a b)
  (progn a b))

(defun gl-app-prog-high-level-primitive (a b)
  (progn a b))


;; OpenGL specific
(defun gl-app-push-matrix ()
  (opengl:gl-push-matrix))

(defun gl-app-pop-matrix ()
  (opengl:gl-pop-matrix))

(defun gl-app-load-identity ()
  (opengl:gl-load-identity))

;; State modifiers
(defun gl-app-point-size (size)
  (opengl:gl-point-size (crop 0.0 100.0 (safe-float-coerce size))))

(defun gl-app-line-width (width)
  (opengl:gl-line-width (crop 0.0 100.0 (safe-float-coerce width))))

(defun gl-app-color (r g b)
  (opengl:gl-color3-f (safe-float-coerce r) (safe-float-coerce g) (safe-float-coerce b)))

(defun gl-app-rotate-2d (angle)
  (opengl:gl-rotatef (safe-float-coerce angle) (safe-float-coerce 0) (safe-float-coerce 0) (safe-float-coerce 1)))

(defun gl-app-translate-2d (x y)
  (opengl:gl-translatef (safe-float-coerce x) (safe-float-coerce y) (safe-float-coerce 0)))

(defun gl-app-scale-2d (x y)
  (opengl:gl-scalef (safe-float-coerce x) (safe-float-coerce y) (safe-float-coerce 0)))

;; Control
;(defmacro gl-app-set-var (a b)
;  `(if (symbolp ,a)
;       (setf ,a ,b)))

;; #FIX
(defmacro gl-app-set-var (a b)
  `(progn 
     (declare (special *gl-app-var-x*) (special *gl-app-var-y*) (special *gl-app-var-z*))
     (if (and (symbolp ,a) 
              (or (equal ,a *gl-app-var-x*) (equal ,a *gl-app-var-y*) (equal ,a *gl-app-var-z*)))
         (setf ,a ,b))))

(defmacro gl-app-repeat-n (n &rest sentences)
  `(dotimes (i (min (safe-int-coerce ,n) *gl-app-max-iterations*))
     (incf *gl-app-current-iterations*)
     (if (<= *gl-app-current-iterations* *gl-app-max-iterations*)
         (progn ,@sentences))))

(defun gl-app-grid-repeat (a b c d)
  nil)

;; Conversion auxiliary functions
(defun safe-float-coerce (x &optional (default 0) (min -10000.0) (max 10000.0))
  (let ((result))
    (if (complexp x)
        ;(setf result (coerce (abs x) 'single-float))
        (setf result (coerce 0 'single-float))
      (setf result (coerce x 'single-float)))
    (safe-crop result min max)))

(defun safe-int-coerce (x &optional (default 0) (min -10000) (max 10000))
  (let ((result))
    (if (complexp x)
        ;(setf result (coerce (floor (abs x)) 'integer))
        (setf result (coerce 0 'integer))
      (setf result (coerce (floor x) 'integer)))
    (safe-crop result min max)))

(defun safe-crop (value min max)
  (let ((result value))
    (if (and min (> min result)) 
        (setf result min))
    (if (and max (< max result)) 
        (setf result max))
    result))
    
        
;;; OpenGL Free Drawing problem definition
(defclass entity-opengl-free-drawing-2d (entity-function)
  ())


(defmethod compiled-program ((o entity-opengl-free-drawing-2d))
  "Answer the compiled function for <o>."
  (compile nil `(lambda () 
                  (declare (special *time-variable*)) 
                  ,(program o))))

;;; Environment auxiliars
(defmethod default-language ((o entity-opengl-free-drawing-2d))
  (system-get 'opengl-free-drawing-default-language))

(defmethod possible-languages ((o entity-opengl-free-drawing-2d))
  (list 
   (system-get 'opengl-free-drawing-default-language)))

(defmethod default-fitness-evaluators ((object entity-opengl-free-drawing-2d))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get 'opengl-free-drawing-default-evaluator)))

(defmethod drawablep ((o entity-opengl-free-drawing-2d))
  "Answer whether <o> can be displayed on the GUI."
  t)

(defmethod draw-in-pixmap (pinboard object pane (o entity-opengl-free-drawing-2d) parent-pinboard x y)
  "Draws object in the pixmap of pinboard interface."
  (not-available-pixmap pinboard object pane o parent-pinboard x y))

(defmethod draw-opengl-on ((o entity-opengl-free-drawing-2d) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (let* ((width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expression (compiled-program o))
         (ix (/ width 10))
         (iy (/ height 10))
         (cx (min width 100))
         (cy (min height 100))
         (dx (/ width cx))
         (dy (/ height cy)))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d 0.d0 100 100 0.d0)      
      (opengl:gl-clear opengl:*GL-ALL-ATTRIB-BITS*)
      (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
      ;(handler-case (eval (program o))
      ;  (error (function) (progn nil))))
      (declare (special *gl-app-current-iterations*))
      (setf *gl-app-current-iterations* 0)
      (eval (program o)))
  (handler-case (opengl:swap-buffers canvas)
    (error (function) (progn nil)))))

(defmethod prepare-children-from ((o entity-opengl-free-drawing-2d) expression algorithm)
  "Prepares <o> to behave like <expression>."
  (declare (ignore algorithm))  
  (setf (program o) expression))

(defmethod constant-p ((o entity-opengl-free-drawing-2d) &optional (check-genotype t) (check-phenotype t))
  "Answers whether <o> is constant."
  nil)

;;; Fitness evaluator
(defclass opengl-free-drawing-evaluator (entity-evaluator)
  ((fitness-function :initarg :fitness-function :accessor fitness-function)))


(defmethod evaluate ((evaluator opengl-free-drawing-evaluator) (object entity-opengl-free-drawing-2d))
  "Use <evaluator> to calculate and answer <object> fitness."
  (funcall (fitness-function evaluator) evaluator object))

(defmethod objective-class ((evaluator opengl-free-drawing-evaluator))
  'entity-opengl-free-drawing-2d)

(defun evaluate-opengl-free-drawing (evaluator object)
  "Evaluation method for OpenGL free drawing object."
  (setf (fitness object) 1)
  1)


;;; Add system objects
(system-add
 (make-instance 'context-free-grammar
                :name 'default-opengl-free-drawing-grammar-2d
                :lexer 'lisp-math-expression-lexer
                :parser-initializer 'initialize-opengl-free-drawing-expression-parser-2d
                :productions (opengl-free-drawing-grammar-productions)
                :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :4-ary-operator 
                                    :draw-operation :draw-expression-list :expression :start-draw)))

(system-add
 (make-instance 'cfg-tree-language 
                :name 'opengl-free-drawing-default-language
				:description "Free drawing language"
                :grammar (system-get-copy 'default-opengl-free-drawing-grammar-2d)
                :simplification-patterns *opengl-free-drawing-editing-patterns-2d*
                :functions (entity-opengl-free-drawing-default-functions-info-2d)
                :terminals '(:constant)
                :variables '(*time-variable*)
                :tokens *opengl-free-drawing-expression-tokens-2d*
                :valid-new-expresion-function 'create-new-random-valid
                :operators (default-genetic-operators-probability-texture-separate)
                :max-size 100
                :max-depth 35))

(system-add
 (make-instance 'opengl-free-drawing-evaluator
                :name 'opengl-free-drawing-default-evaluator
                :description "OpenGL free drawing default evaluator"
                :fitness-function 'evaluate-opengl-free-drawing
                :min-fitness 0
                :max-fitness 10
                :solution-fitness 9.8))


(system-add-default-objects 
 ;; Some lines
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-1
                :description "OpenGL Free drawing 1"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LINE-WIDTH 15)
                                                                  (GL-APP-COLOR 0.3 0 1)
                                                                  (GL-APP-TRANSLATE-2D 0 0)
                                                                  (GL-APP-LINE-2D 0 0 100 100)
                                                                  (GL-APP-LINE-2D 100 0 0 0)
                                                                  (GL-APP-COLOR 0.3 1 1)
                                                                  (GL-APP-LINE-2D 0 100 0 0)
                                                                  (GL-APP-LINE-2D 0 100 100 0)
                                                                  (GL-APP-LINE-2D 100 100 100 0)
                                                                  (GL-APP-LINE-2D 0 100 100 100)))))
 ;; Some quads 1
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-2
                :description "OpenGL Free drawing 2"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW 
                                                      (gl-app-point-size 15)
                                                      (gl-app-point-2d 20 20)))))
 ;; Some quads 2
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-3
                :description "OpenGL Free drawing 3"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW 
                                                      (gl-app-point-2d 20 20)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)
                                                      (gl-app-scale-2d 0.5 0.5)
                                                      (gl-app-color 1 0.5 1)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)
                                                      (gl-app-color 1 0.5 0.2)
                                                      (gl-app-quad-2d 0 0 0 100 10 100 100 0)
                                                      (gl-app-scale-2d 0.5 0.5)
                                                      (gl-app-color 0.5 0.5 1.0)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)
                                                      (gl-app-color 0.5 0.5 1.0)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)))))
 ;; Some quads 3
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-4
                :description "OpenGL Free drawing 4"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW 
                                                      (gl-app-point-2d 20 20)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)
                                                      (gl-app-scale-2d 0.5 0.5)
                                                      (gl-app-color 1 0.5 1)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)
                                                      (gl-app-color 1 0.5 0.2)
                                                      (gl-app-quad-2d 0 0 0 100 10 100 100 0)
                                                      (gl-app-scale-2d 0.5 0.5)
                                                      (gl-app-color 0.5 0.5 1.0)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)
                                                      (gl-app-color 0.5 0.5 1.0)
                                                      (gl-app-quad-2d 0 0 0 100 100 100 100 0)))))
 ;; Some gl-begin lists
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-5
                :description "OpenGL Free drawing 5"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (gl-app-begin-lines-2d
                                                                   (gl-app-rotate-2d 90)
                                                                   (gl-app-vertex-2d 0 100)
                                                                   (gl-app-rotate-2d 90)
                                                                   (gl-app-vertex-2d 100 0)
                                                                   (gl-app-rotate-2d 90)
                                                                   (gl-app-vertex-2d 0 100)
                                                                   (gl-app-rotate-2d 90)
                                                                   (gl-app-vertex-2d 50 0))))))
 ;; Some iterators
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-circle-of-points-1
                :description "OpenGL Free drawing 4"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '
                                         (GL-APP-DRAW (GL-APP-LOAD-IDENTITY)
                                                      (gl-app-translate-2d 50 50)
                                                      (GL-APP-POINT-SIZE 2)
                                                      (GL-APP-REPEAT-N 100
                                                                       (GL-APP-ROTATE-2D 24)
                                                                       (GL-APP-POINT-2D 10 0))))))
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-circle-of-points-2
                :description "OpenGL Free drawing 5"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-COLOR (ABS (SIN (* 4 *TIME-VARIABLE*)))
                                                                    (ABS (SIN (* 2 *TIME-VARIABLE*)))
                                                                    (ABS (SIN *TIME-VARIABLE*)))
                                                      (GL-APP-LOAD-IDENTITY)
                                                      (GL-APP-TRANSLATE-2D 50 50)
                                                      (GL-APP-POINT-SIZE 2)
                                                      (GL-APP-SCALE-2D (* 4 (ABS (SIN *TIME-VARIABLE*)))
                                                                       (* 4 (ABS (SIN *TIME-VARIABLE*))))
                                                      (GL-APP-REPEAT-N 10 
                                                                       (GL-APP-ROTATE-2D 4) 
                                                                       (GL-APP-POINT-2D 10 10))
                                                      (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D 4) 
                                                                       (GL-APP-POINT-2D 10 10))))))
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-circle-of-points-3
                :description "OpenGL Free drawing 6"
                :subject (default-object-in-search
                          (make-instance 
                           'entity-opengl-free-drawing-2d
                           :expresion '(GL-APP-DRAW 
                                        (GL-APP-COLOR (ABS (SIN (* 4 *TIME-VARIABLE*)))
                                                      (ABS (SIN (* 2 *TIME-VARIABLE*)))
                                                      (ABS (SIN *TIME-VARIABLE*)))
                                        (GL-APP-LOAD-IDENTITY)
                                        (GL-APP-TRANSLATE-2D 50 50)
                                        (GL-APP-POINT-SIZE 15)
                                        (GL-APP-SCALE-2D (ABS (SIN *TIME-VARIABLE*))
                                                         (ABS (SIN *TIME-VARIABLE*)))
                                        (GL-APP-REPEAT-N 100
                                                         (GL-APP-ROTATE-2D 12)
                                                         (GL-APP-LINE-2D 33
                                                                         10
                                                                         (* 41 (ABS (SIN *TIME-VARIABLE*)))
                                                                         (* 11 (* 
                                                                                (ABS (COS *TIME-VARIABLE*))
                                                                                (ABS (SIN *TIME-VARIABLE*))))))
                                        (GL-APP-REPEAT-N 100
                                                         (GL-APP-ROTATE-2D 12)
                                                         (GL-APP-LINE-2D 33
                                                                         10
                                                                         (* 41 (ABS (SIN *TIME-VARIABLE*)))
                                                                         (* 11 (* 
                                                                                (ABS (COS *TIME-VARIABLE*))
                                                                                (ABS (SIN *TIME-VARIABLE*))))))))))
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-circle-of-points-4
                :description "OpenGL Free drawing 7"
                :subject (default-object-in-search
                          (make-instance 
                           'entity-opengl-free-drawing-2d
                           :expresion '(GL-APP-DRAW (GL-APP-COLOR (ABS (SIN (* 4 *TIME-VARIABLE*)))
                                                                  (ABS (SIN (* 2 *TIME-VARIABLE*)))
                                                                  (ABS (SIN *TIME-VARIABLE*)))
                                                    (GL-APP-LOAD-IDENTITY)
                                                    (GL-APP-TRANSLATE-2D 50 50)
                                                    (GL-APP-POINT-SIZE 15)
                                                    (GL-APP-SCALE-2D (ABS (SIN *TIME-VARIABLE*))
                                                                     (ABS (SIN *TIME-VARIABLE*)))
                                                    (GL-APP-REPEAT-N 100
                                                                     (GL-APP-ROTATE-2D 12)
                                                                     (GL-APP-LINE-2D 33
                                                                                     10
                                                                                     (* 41 (ABS (SIN *TIME-VARIABLE*)))
                                                                                     (* 11
                                                                                        (* 
                                                                                        (ABS (COS *TIME-VARIABLE*))
                                                                                        (ABS (SIN *TIME-VARIABLE*))))))
                                                    (GL-APP-REPEAT-N 100
                                                                     (GL-APP-ROTATE-2D 12)
                                                                     (GL-APP-LINE-2D 33
                                                                                     10
                                                                                     (* 41 (ABS (SIN *TIME-VARIABLE*)))
                                                                                     (* 11
                                                                                        (* 
                                                                                        (ABS (COS *TIME-VARIABLE*))
                                                                                        (ABS (SIN *TIME-VARIABLE*))))))))))
 ;; Some variable 
 
 ;; Some animations
 )




#|
;; TEST FOR PARSING EXPRESSIONS

(progn 
  (setf gg
        (make-instance 'context-free-grammar
                       :name 'default-opengl-free-drawing-grammar
                       :lexer 'lisp-math-expression-lexer
                       :parser-initializer 'initialize-opengl-free-drawing-expression-parser-2d
                       :productions (opengl-free-drawing-grammar-productions)
                       :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :4-ary-operator :vertex-expression :expression)))
  (setf ll
        (make-instance 'cfg-tree-language 
                       :name 'opengl-free-drawing-default-language
                       :description "Free drawing language"
                       :grammar gg
                       :simplification-patterns *opengl-free-drawing-editing-patterns-2d*
                       :functions (entity-opengl-free-drawing-default-functions-info-2d)
                       :terminals '(:constant)
                       :variables '(*time-variable*)
                       :tokens *opengl-free-drawing-expression-tokens-2d*
                       :valid-new-expresion-function 'create-new-random-valid
                       :operators (default-genetic-operators-probability-texture-separate)
                       :max-size 80
                       :max-depth 16))
  (setf pe '(GL-APP-DRAW (GL-APP-LINE-WIDTH 15)))
  (parse gg pe))


(setf pe '(GL-APP-DRAW (GL-APP-LINE-WIDTH 15)))

(setf pe '(GL-APP-DRAW (GL-APP-REPEAT-N 100
                                        (GL-APP-ROTATE-2D 12)
                                        (GL-APP-LINE-2D 33 10 10 10))))

(setf pe '(GL-APP-DRAW 
           (gl-app-point-2d 20 20)
           (gl-app-quad-2d 0 0 0 100 100 100 100 0)
           (gl-app-scale-2d 0.5 0.5)
           (gl-app-color 1 0.5 1)
           (gl-app-quad-2d 0 0 0 100 100 100 100 0)
           (gl-app-color 1 0.5 0.2)
           (gl-app-quad-2d 0 0 0 100 10 100 100 0)
           (gl-app-scale-2d 0.5 0.5)
           (gl-app-color 0.5 0.5 1.0)
           (gl-app-quad-2d 0 0 0 100 100 100 100 0)
           (gl-app-color 0.5 0.5 1.0)
           (gl-app-quad-2d 0 0 0 100 100 100 100 0)))
|#