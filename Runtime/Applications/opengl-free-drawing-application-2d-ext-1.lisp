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

(load-application-file "Applications\\opengl-free-drawing-evaluator-image-distance.lisp")

;; OpenGL free drawing language definition 
(defparameter *opengl-free-drawing-expression-tokens-2d-ext-1*
  '(;; Keywords
    (gl-app-draw :draw)
    ;; Low level drawing
    (gl-app-begin-points-2d :gl-app-begin-point-primitive)
    (gl-app-begin-lines-2d :gl-app-begin-point-primitive)
    (gl-app-begin-triangles-2d :gl-app-begin-point-primitive)
    (gl-app-begin-triangles-strip-2d :gl-app-begin-point-primitive)
    (gl-app-begin-quads-2d :gl-app-begin-point-primitive)
    (gl-app-begin-quads-strip-2d :gl-app-begin-point-primitive)
    (gl-app-begin-polygon-2d :gl-app-begin-point-primitive)
    (gl-app-point-primitive :gl-app-point-primitive-2d)
    (gl-app-color-primitive :gl-app-color-primitive-2d)
    ;; OpenGL specific
    (gl-app-push-matrix :push-matrix)
    (gl-app-pop-matrix :pop-matrix)
    (gl-app-load-identity :load-identity)
    ;; High level drawing
    (gl-app-point-2d :2-ary-high-primitive-operator)
    (gl-app-line-2d :4-ary-high-primitive-operator)
    (gl-app-triangle-2d :6-ary-high-primitive-operator)
    (gl-app-quad-2d :8-ary-high-primitive-operator)
    ;; Graphic state modifiers
    (gl-app-point-size :1-ary-high-primitive-operator)
    (gl-app-line-width :1-ary-high-primitive-operator)
    ;; Vertex operations
    (gl-app-vertex-2d :2-ary-vertex-operator)
    (gl-app-color :3-ary-vertex-operator)
    (gl-app-rotate-2d :1-ary-vertex-operator)
    (gl-app-translate-2d :2-ary-vertex-operator)
    (gl-app-scale-2d :2-ary-vertex-operator)
    ;; Expression to vertex conversion
    (gl-app-cvec2 :cvec2)
    ;; Control
    (gl-app-repeat-n :2-ary-control-operator)
    (gl-app-repeat-x-y :4-ary-control-operator)
    (gl-app-repeat-var :1-ary-operator)
    (gl-app-set-var :set-var)
    ;; Functions
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (asin :1-ary-operator)
    (cos :1-ary-operator)
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
    (*gl-app-i* :iteration-var)
    (*gl-app-j* :iteration-var)
    (*gl-app-k* :iteration-var)))

(defun initialize-opengl-free-drawing-expression-parser-2d-ext-1 (name)
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
               ;; Draw operation: points primitives
               ((draw-operation :open draw-point-primitive :close)
                `(:draw-operation (,$2)))
               ((draw-point-primitive :gl-app-begin-point-primitive draw-point-list)
                `(:draw-point-primitive ,$1 ,$2))
               ((draw-point-list draw-point)
                `(:draw-point-list ,$1))
               ((draw-point-list draw-point draw-point-list)
                `(:draw-point-list ,$1 ,$2))
               ;; Begin / end pair primitives
               ((draw-point :open :gl-app-point-primitive-2d expression expression :close)
                `(:draw-point (,$2 ,$3 ,$4)))
               ((draw-point :open :gl-app-color-primitive-2d expression expression expression :close)
                `(:draw-point (,$2 ,$3 ,$4 ,$5)))
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
               ;; Terminals
               ((expression :var)
                `(:expression ,$1))
               ((expression :time-var)
                `(:expression ,$1))
               ((expression :iteration-var)
                `(:expression ,$1))
               ((expression :constant)
                `(:expression ,$1)))))

(defun opengl-free-drawing-grammar-productions-ext-1 ()
  '((start start-draw)
    (start-draw :open :draw draw-expression-list :close)
    ;; Draw operation list
    (draw-expression-list draw-operation)
    (draw-expression-list draw-operation draw-expression-list)
    ;; Draw operation: points primitives
    (draw-operation :open draw-point-primitive :close)
    (draw-point-primitive :gl-app-begin-point-primitive draw-points-list)
    (draw-point-list draw-point)
    (draw-point-list draw-point draw-point-list)
    (draw-point :open :gl-app-point-primitive-2d expression expression :close)
    ;; Draw operation: setting colors
    (draw-point :open :gl-app-color-primitive-2d expression expression expression :close)
    ;; Draw operations: opengl matrix
    (draw-operation :open :load-identity :close)
    (draw-operation :open :push-matrix :close)
    (draw-operation :open :pop-matrix :close)
    ;; Draw operations: primitives
    (draw-operation :open :1-ary-high-primitive-operator expression :close)
    (draw-operation :open :2-ary-high-primitive-operator expression expression :close)
    (draw-operation :open :set-var :var expression :close)
    (draw-operation :open :6-ary-high-primitive-operator 
                    expression expression expression expression expression expression :close)
    (draw-operation :open :8-ary-high-primitive-operator 
                    expression expression expression expression expression expression expression expression :close)
    ;; Control operations
    (draw-operation :open :2-ary-control-operator expression draw-expression-list :close)
    ;; Expression
    (expression :open :1-ary-operator expression :close)
    (expression :open :2-ary-operator expression expression :close)
    ;; Terminals
    (expression :var)
    (expression :constant)))

(defun entity-opengl-free-drawing-default-functions-info-2d-ext-1 ()
  '((gl-app-draw 2)
    ;; Low level drawing
    (gl-app-point-primitive 2)
    (gl-app-color-primitive 3)
    ;; OpenGL specific
    (gl-app-push-matrix 0)
    (gl-app-pop-matrix 0)
    (gl-app-load-identity 0)
    ;; High level drawing
    (gl-app-point-2d 2)
    (gl-app-line-2d 4)
    (gl-app-triangle-2d 6)
    (gl-app-quad-2d 8)
    ;; Graphic state modifiers
    (gl-app-point-size 1)
    (gl-app-line-width 1)
    ;; Vertex operations
    (gl-app-color 3)
    (gl-app-vertex-2d 2)
    (gl-app-rotate-2d 1)
    (gl-app-translate-2d 2)
    (gl-app-scale-2d 2)
    ;; Expression to vertex conversion
    (gl-app-cvec2 2)
    ;; Control
    (gl-app-repeat-n 2)
    (gl-app-repeat-x-y 4)
    (gl-app-repeat-var 1)
    (gl-app-set-var 2)
    ;; Math functions
    (abs 1)
    (sin 1)
    (asin 1)
    (cos 1)
    (+ 2)
    (- 2)
    (* 2)
    (/- 2)))

;; OpenGL Free Drawing Ext-1 specific functions
;; Low level primitives
(defun gl-app-begin-points-2d (&rest args)
  `(progn 
     (opengl:gl-begin opengl:*gl-points*)
      ,args
     (opengl:gl-end)))

(defmacro gl-app-begin-lines-2d (&rest args)
  `(progn 
     (opengl:gl-begin opengl:*gl-line-loop*)
      ,@args
     (opengl:gl-end)))

(defmacro gl-app-begin-triangles-2d (&rest args)
  `(progn
     (opengl:gl-begin opengl:*gl-triangles*)
     ,@args
     (opengl:gl-end)))

(defmacro gl-app-begin-triangles-strip-2d (&rest args)
  `(progn
     (opengl:gl-begin opengl:*gl-triangle-strip*)
     ,@args
     (opengl:gl-end)))

(defmacro gl-app-begin-triangles-fan-2d (&rest args)
  `(progn
     (opengl:gl-begin opengl:*gl-triangle-fan*)
     ,@args
     (opengl:gl-end)))

(defmacro gl-app-begin-quads-2d (&rest args)
  `(progn
     (opengl:gl-begin opengl:*gl-quads*)
     ,@args
     (opengl:gl-end)))

(defmacro gl-app-begin-quads-strip-2d (&rest args)
  `(progn
     (opengl:gl-begin opengl:*gl-quad-strip*)
     ,@args
     (opengl:gl-end)))

(defmacro gl-app-begin-polygon-2d (&rest args)
  `(progn
     (opengl:gl-begin opengl:*gl-polygon*)
     ,@args
     (opengl:gl-end)))

;; Basic primitives
(defun gl-app-point-primitive (x y)
  (gl-app-vertex-2d x y))

(defun gl-app-color-primitive (r g b)
  (gl-app-color r g b))

;;; Environment auxiliars
(defmethod possible-languages ((o entity-opengl-free-drawing-2d))
  (list 
   (system-get 'opengl-free-drawing-default-language)
   (system-get 'opengl-free-drawing-default-language-ext-1)))

;;; Add system objects
(system-add
 (make-instance 'context-free-grammar
                :name 'default-opengl-free-drawing-grammar-2d-ext-1
                :lexer 'lisp-math-expression-lexer
                :parser-initializer 'initialize-opengl-free-drawing-expression-parser-2d-ext-1
                :productions (opengl-free-drawing-grammar-productions-ext-1)
                :crossover-nodes '(:1-ary-operator :2-ary-operator 
                                    :draw-operation :draw-expression-list :expression :start-draw)))

(system-add
 (make-instance 'cfg-tree-language 
                :name 'opengl-free-drawing-default-language-ext-1
				:description "Free drawing language x1"
                :grammar (system-get-copy 'default-opengl-free-drawing-grammar-2d-ext-1)
                :simplification-patterns *opengl-free-drawing-editing-patterns-2d*
                :functions (entity-opengl-free-drawing-default-functions-info-2d-ext-1)
                :terminals '(:constant)
                :variables '(*time-variable*)
                :tokens *opengl-free-drawing-expression-tokens-2d-ext-1*
                :valid-new-expresion-function 'random-create-cfg-initial-size
                :operators (default-genetic-operators-probability-gl-app-free-drawing)
                :max-size 100
                :max-depth 35))

#|
(defun interface-for-opengl-evaluator ()
  (make-instance 'PANE-EDITOR-ENTITY-OPENGL))
|#

(defun interface-for-opengl-evaluator ()
  (unless *opengl-evaluator-pane*
    (setf *opengl-evaluator-pane* (make-instance 'PANE-EDITOR-ENTITY-OPENGL)))
  (open *opengl-evaluator-pane*)
  *opengl-evaluator-pane*)

(defun calculate-image-distance (evaluator object)
  (let ((total 1))
    (dolist (value (image-pixels evaluator))
      (incf total (abs (- value (fli:dereference pp :index i)))))
    total))

;; #EXT OVERRIDE
(defmethod default-fitness-evaluators ((object entity-opengl-free-drawing-2d))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get 'opengl-free-drawing-default-evaluator)
   (system-get 'opengl-free-drawing-evaluator-image-distance)))

;; Add system object
(system-add
 (make-instance 'opengl-free-drawing-evaluator-image-distance
                :name 'opengl-free-drawing-evaluator-image-distance
                :description "OpenGL free drawing default evaluator image"
                :fitness-function 'evaluate-opengl-free-drawing
                :min-fitness 0
                :max-fitness 10
                :solution-fitness 9.8))


(system-add-default-objects 
 ;; Test example
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-1
                :description "OpenGL Free drawing X1 1"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(gl-app-draw
                                                      (gl-app-color 0.5 0.5 1.0)
                                                      (gl-app-begin-triangles-2d
                                                       (gl-app-point-primitive 0 0)
                                                       (gl-app-color-primitive 0.5 0.0 1.0)
                                                       (gl-app-point-primitive 0 100)
                                                       (gl-app-color-primitive 0.0 0.5 1.0)
                                                       (gl-app-point-primitive 20 0))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
 (make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-2
                :description "OpenGL Free drawing X1 2"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 (SIN (* 1 1))) (GL-APP-COLOR 1 0 0) (GL-APP-POINT-SIZE 3) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D 50) (GL-APP-TRANSLATE-2D 50 (ABS 1)) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 1 (GL-APP-COLOR 0.99 1 0.2) (GL-APP-LINE-WIDTH 4) (GL-APP-POINT-2D (SIN 1) (COS (/- (SIN 1) 0.04927234))) (GL-APP-QUAD-2D 0.12020743 (- (- (+ (* (SIN (- 1 0.32623553)) 0.040468615) (/- (SIN 1) (/- (SIN 1) 0.04927234))) (/- (ASIN (* 1 (/- (* 1 (- 0.15597373 0.45042896)) 1))) (COS 1))) 1) 50 0.5629076 1 0.7879724 (SIN (+ 0.45167845 (COS 0.35967723))) 100) (GL-APP-ROTATE-2D 24) (GL-APP-ROTATE-2D 24) (GL-APP-SET-VAR 1 (SIN 0.32022417))))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-3
                :description "OpenGL Free drawing X1 3"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-REPEAT-N 16 (GL-APP-ROTATE-2D 24) (GL-APP-ROTATE-2D 0.40524718) (GL-APP-POINT-2D (GL-APP-REPEAT-VAR 1) 24)) (GL-APP-ROTATE-2D 24) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-POINT-SIZE 0.6217792) (GL-APP-REPEAT-N 50 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0 (/- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-ROTATE-2D 24) (GL-APP-ROTATE-2D 24) (GL-APP-ROTATE-2D 0.40524718) (GL-APP-POINT-2D (GL-APP-REPEAT-VAR 1) 24))))))
                (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-4
                :description "OpenGL Free drawing X1 4"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0 (/- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-REPEAT-N 16 (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0 (- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-ROTATE-2D 1) (GL-APP-POINT-2D 0.83541346 24))) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0.49401867 (/- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-ROTATE-2D 24) (GL-APP-POINT-2D (GL-APP-REPEAT-VAR 1) 24))))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-5
                :description "OpenGL Free drawing X1 5"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion 'nil)
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-6
                :description "OpenGL Free drawing X1 6"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0 (/- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-ROTATE-2D 24) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-POINT-SIZE 5) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0 (/- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-ROTATE-2D 16) (GL-APP-POINT-2D (GL-APP-REPEAT-VAR 1) 0))))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-7
                :description "OpenGL Free drawing X1 7"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 24)) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 24)) (GL-APP-POINT-2D (SIN 1) (COS (/- (SIN 1) 0.04927234))) (GL-APP-QUAD-2D 0.12020743 (- (- (+ (* (SIN (- 1 0.32623553)) 0.040468615) (/- (SIN 1) 0.04927234)) (/- (ASIN (* 1 (/- (* 1 (- 0.15597373 0.45042896)) 1))) (COS 1))) 1) 50 0.5629076 1 0.7879724 (SIN (+ 0.45167845 (COS 0.35967723))) 100) (GL-APP-ROTATE-2D 24) (GL-APP-ROTATE-2D 24) (GL-APP-SET-VAR 1 (SIN 0.32022417))))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-8
                :description "OpenGL Free drawing X1 8"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-COLOR 0.6754998 0.1250208 0.22457358) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 24)) (GL-APP-TRANSLATE-2D 50 0.54186124) (GL-APP-POINT-SIZE 1) (GL-APP-REPEAT-N 100 (GL-APP-REPEAT-N 1 (GL-APP-ROTATE-2D (+ 50 (COS 0.9510537))) (GL-APP-ROTATE-2D (+ 0.43685555 (COS 0.9510537))) (GL-APP-POINT-2D (SIN 1) (ABS (/- 0.5015703 0.04927234))) (GL-APP-COLOR 0.72847206 0.4295278 0.07343382)) (GL-APP-COLOR 0.6204523 0.941407 0.22754549) (GL-APP-COLOR 0.96359444 0.13132859 (- (SIN (SIN 0.59854627)) 1)) (GL-APP-REPEAT-N 1 (GL-APP-COLOR 0.73868245 0.035978623 0.6927126) (GL-APP-POINT-2D (SIN 1) (ABS (/- (SIN 1) 0.04927234))) (GL-APP-COLOR 0.20848647 0.4295278 0.07343382))))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-9
                :description "OpenGL Free drawing X1 9"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 5) (GL-APP-REPEAT-N 16 (GL-APP-COLOR (- 1 (/- (GL-APP-REPEAT-VAR 1) 16)) 0 (/- (GL-APP-REPEAT-VAR 1) 16)) (GL-APP-ROTATE-2D 24) (GL-APP-POINT-2D 40 0))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-10
                :description "OpenGL Free drawing X1 10"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 50)) (GL-APP-TRANSLATE-2D 2 (GL-APP-REPEAT-VAR (+ (ABS 0.95134205) 0.2058494))) (GL-APP-TRIANGLE-2D (GL-APP-REPEAT-VAR 50) (GL-APP-REPEAT-VAR (ABS (ASIN *TIME-VARIABLE*))) 0.4790219 (ABS (ASIN *TIME-VARIABLE*)) (- (COS (ASIN (ABS (- 0.92102784 *TIME-VARIABLE*)))) *TIME-VARIABLE*) (ASIN *TIME-VARIABLE*)) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D 0.9165687) (GL-APP-POINT-2D 0.95134205 0)))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-11
                :description "OpenGL Free drawing X1 11"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 50)) (GL-APP-TRANSLATE-2D (GL-APP-REPEAT-VAR *TIME-VARIABLE*) (GL-APP-REPEAT-VAR (+ (ABS 0.95134205) 0.2058494))) (GL-APP-TRIANGLE-2D (GL-APP-REPEAT-VAR 50) (GL-APP-REPEAT-VAR (ABS (ASIN *TIME-VARIABLE*))) 0.4790219 (ABS (ASIN *TIME-VARIABLE*)) (- (GL-APP-REPEAT-VAR (ABS (ASIN *TIME-VARIABLE*))) *TIME-VARIABLE*) (ASIN *TIME-VARIABLE*)) (GL-APP-COLOR 0.93181205 0.96525383 0.021565903))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-12
                :description "OpenGL Free drawing X1 12"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                          :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 50)) (GL-APP-TRANSLATE-2D (GL-APP-REPEAT-VAR *TIME-VARIABLE*) (GL-APP-REPEAT-VAR 0.95134205)) (GL-APP-TRIANGLE-2D (GL-APP-REPEAT-VAR *TIME-VARIABLE*) (GL-APP-REPEAT-VAR (ABS (ASIN *TIME-VARIABLE*))) 0.4790219 (ABS (ASIN *TIME-VARIABLE*)) (- (GL-APP-REPEAT-VAR (ABS (ASIN *TIME-VARIABLE*))) *TIME-VARIABLE*) (ASIN *TIME-VARIABLE*)) (GL-APP-COLOR 0.93181205 0.96525383 0.021565903))))
                          (system-get 'opengl-free-drawing-default-language-ext-1)))
(make-instance 'registrable-object-wrapper 
                :name 'opengl-free-draw-x1-13
                :description "OpenGL Free drawing X1 13"
                :subject (default-object-in-search
                          (make-instance 'entity-opengl-free-drawing-2d
                                         :expresion '(GL-APP-DRAW (GL-APP-LOAD-IDENTITY) (GL-APP-TRANSLATE-2D 50 50) (GL-APP-POINT-SIZE 2) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D (ABS 50)) (GL-APP-ROTATE-2D 0) (GL-APP-TRANSLATE-2D 2 (GL-APP-REPEAT-VAR (+ (ABS 0.95134205) 0.2058494))) (GL-APP-TRIANGLE-2D (GL-APP-REPEAT-VAR 50) (GL-APP-REPEAT-VAR (ABS (ASIN *TIME-VARIABLE*))) 0.4790219 (ABS (ASIN *TIME-VARIABLE*)) (- (COS (ASIN (ABS (- 0.92102784 *TIME-VARIABLE*)))) *TIME-VARIABLE*) (ASIN *TIME-VARIABLE*)) (GL-APP-REPEAT-N 100 (GL-APP-ROTATE-2D 0.4827907) (GL-APP-POINT-2D (ABS (+ (GL-APP-REPEAT-VAR *TIME-VARIABLE*) (* (/- *TIME-VARIABLE* (- 7.924214E-4 (COS *TIME-VARIABLE*))) (/- *TIME-VARIABLE* (ABS 0.3182265))))) 0)))))
                          (system-get 'opengl-free-drawing-default-language-ext-1))))

#|
;; TEST FOR PARSING EXPRESSIONS

(progn 
  (setf gg
        (make-instance 'context-free-grammar
                       :name 'default-opengl-free-drawing-grammar-2d-ext-1
                       :lexer 'lisp-math-expression-lexer
                       :parser-initializer 'initialize-opengl-free-drawing-expression-parser-2d-ext-1
                       :productions (opengl-free-drawing-grammar-productions-ext-1)
                       :crossover-nodes '(:1-ary-operator :2-ary-operator 
                                           :vertex-expression :expression)))
  (setf ll
        (make-instance 'cfg-tree-language 
                       :name 'opengl-free-drawing-default-language-ext-1
                       :grammar gg
                       :simplification-patterns *opengl-free-drawing-editing-patterns-2d*
                       :functions (entity-opengl-free-drawing-default-functions-info-2d-ext-1)
                       :terminals '(:constant)
                       :variables '(*time-variable*)
                       :tokens *opengl-free-drawing-expression-tokens-2d-ext-1*
                       :valid-new-expresion-function 'create-new-random-valid
                       :operators (default-genetic-operators-probability-texture-separate)
                       :max-size 80
                       :max-depth 16))
  (setf pe '(GL-APP-DRAW 
             (GL-APP-COLOR 0.5 0.5 1.0)
             (GL-APP-BEGIN-TRIANGLES-2D (gl-app-point-primitive 0 0)
                                        (gl-app-color-primitive 0.5 0.0 1.0)
                                        (gl-app-point-primitive 0 111)
                                        (gl-app-color-primitive 0.0 0.5 1.0)
                                        (gl-app-point-primitive 211 0))))
  (parse gg pe))



(setf pe '(GL-APP-DRAW (GL-APP-LINE-WIDTH 15)))

(setf pe '(GL-APP-DRAW (GL-APP-REPEAT-N 100
                                        (GL-APP-ROTATE-2D 12)
                                        (GL-APP-LINE-2D 33 10 10 10))))

(setf pe '(GL-APP-DRAW 
           (GL-APP-COLOR 0.5 0.5 1.0)
           (GL-APP-BEGIN-TRIANGLES-2D (gl-app-point-primitive 0 0)
                                      (gl-app-color-primitive 0.5 0.0 1.0)
                                      (gl-app-point-primitive 0 111)
                                      (gl-app-color-primitive 0.0 0.5 1.0)
                                      (gl-app-point-primitive 211 0))))
|#



