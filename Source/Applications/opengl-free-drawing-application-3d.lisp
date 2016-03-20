;;; Language for expressions which repressent a free opengl expressions
;;;
;;;  - 3d drawing set
;;;  - points, lines, triangle, quads, polygon & strips
;;;  - push, pop, load-identity matrix operations
;;;  - color, texture
;;;  - translate, scale, rotate
;;;  - x, y and z variables can be used
;;;  - cube, ball, circle and cilynder
;;;  - +, -, *, /-, abs, cos, sin, tan functions
;;;


;; OpenGL free drawing language definition 
(defparameter *opengl-free-drawing-expression-tokens-3d*
  '(;; Keywords
    (gl-app-draw :draw)
    ;; Basic figures
    (gl-app-point :gl-app-point)
    (gl-app-line :gl-app-line)
    (gl-app-triangle :gl-app-triangle)
    (gl-app-triangle-strip :gl-app-triangle-strip)
    (gl-app-quad :gl-app-quad)
    (gl-app-quad-strip :gl-app-quad-strip)
    (gl-app-polygon :gl-app-polygon)
    ;; OpenGL specific
    (gl-app-push-matrix :push-matrix)
    (gl-app-pop-matrix :pop-matrix)
    (gl-app-load-identity :load-identity)
    (gl-app-set-matrix-mode :set-matrix-mode)
    (gl-app-projection-matrix :matrix-mode)
    (gl-app-modelview-matrix :matrix-mode)
    ;; > level figures
    (gl-app-cube  :1-ary-operator)
    (gl-app-ball :2-ary-operator)
    (gl-app-circle :3-ary-operator)
    (gl-app-cylinder :3-ary-operator)
    ;; Graphic state modifiers
    (gl-app-point-size :1-ary-primitive-operator)
    (gl-app-line-width :1-ary-primitive-operator)
    ;; State modifiers
    (gl-app-color :1-ary-primitive-operator)
    (gl-app-rotate :1-ary-primitive-operator)
    (gl-app-translate :1-ary-primitive-operator)
    (gl-app-scale :1-ary-primitive-operator)
    ;; Control
    (gl-app-repeat-n :2-ary-operator)
    (gl-app-grid-repeat :4-ary-operator)
    (gl-app-set-var :2-ary-operator)
    ;; Functions
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (asin :1-ary-operator)
    (cos :1-ary-operator)
    (tan :1-ary-operator)
    (atan :1-ary-operator)
    (sqr :1-ary-operator)
    ;; Operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/- :2-ary-operator)
    (mod :2-ary-operator)
    (div :2-ary-operator)))


(defun initialize-opengl-free-drawing-expression-parser-3d (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start :open :draw draw-expression-list :close)
                `(,$3 ,$4 ,$5))
               ;; Draw operation list
               ((draw-operation-list :open draw-operation :close)
                `(:expresion (,$2)))
               ((draw-operation-list :open draw-operation draw-operation-list :close)
                `(:expresion (,$2 ,$3)))
               ;; Draw operations: opengl matrix
               ((draw-operation :open :set-matrix-mode :matrix-mode :close)
                `(:expresion (,$2 ,$3)))
               ((draw-operation :open :push-matrix :close)
                `(:expresion (,$2)))
               ((draw-operation :open :pop-matrix :close)
                `(:expresion (,$2)))
               ;; Draw operations: primitives
               ((draw-operation :open :1-ary-primitive-operator vertex-expresion :close)
                `(:expresion (,$2 ,$3)))
               ((draw-operation :open :2-ary-primitive-operator vertex-expresion vertex-expresion :close)
                `(:expresion (,$2 ,$3 ,$4)))
               ((draw-operation 
                 :open :3-ary-primitive-operator vertex-expresion vertex-expresion vertex-expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5)))
               ((draw-operation 
                 :open 
                 :4-ary-primitive-operator vertex-expresion vertex-expresion vertex-expresion vertex-expresion 
                 :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5 ,$6)))
               ;; Expression
               ((expresion :open :1-ary-operator expresion :close)
                `(:expresion (,$2 ,$3)))
               ((expresion :open :2-ary-operator expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4)))
               ((expresion :open :3-ary-operator expresion expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5)))
               ((expresion :open :4-ary-operator expresion expresion expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5 ,$6)))
               ((expresion :var)
                `(:expresion ,$1))
               ((expresion :number)
                `(:expresion ,$1))
               ;; Vertex expression
               ((vertex-expression expresion expresion expresion)
                `(:vertex-expression ,$1 ,$2 ,$3))
               ((vertex-expression :open :1-ary-vertex-operator vertex-expression :close)
                `(vertex-expression ,$2 ,$3))
               ((vertex-expression :open :2-ary-vertex-operator vertex-expression vertex-expression :close)
                `(vertex-expression ,$2 ,$3 ,$4)))))

(defun opengl-free-drawing-grammar-productions-3d ()
  '((start :open :draw draw-expression-list :close)
    ;; Draw operation list
    (draw-operation-list :open draw-operation :close)
    (draw-operation-list :open draw-operation draw-operation-list :close)
    ;; Draw operations: opengl matrix
    (draw-operation :open :set-matrix-mode :matrix-mode :close)
    (draw-operation :open :push-matrix :close)
    (draw-operation :open :pop-matrix :close)
    ;; Draw operations: primitives
    (draw-operation :open :1-ary-primitive-operator vertex-expresion :close)
    (draw-operation :open :2-ary-primitive-operator vertex-expresion vertex-expresion :close)
    (draw-operation :open :3-ary-primitive-operator vertex-expresion vertex-expresion vertex-expresion :close)
    (draw-operation :open :4-ary-primitive-operator vertex-expresion vertex-expresion vertex-expresion vertex-expresion                    :close)
    ;; Expression
    (expresion :open :1-ary-operator expresion :close)
    (expresion :open :2-ary-operator expresion expresion :close)
    (expresion :open :3-ary-operator expresion expresion expresion :close)
    (expresion :open :4-ary-operator expresion expresion expresion expresion :close)
    (expresion :var)
    (expresion :number)
    ;; Vertex expression
    (vertex-expression expresion expresion expresion)
    (vertex-expression :open :1-ary-vertex-operator vertex-expression :close)
    (vertex-expression :open :2-ary-vertex-operator vertex-expression vertex-expression :close)))
   

(defun entity-opengl-free-drawing-default-functions-info-3d ()
  '(
    #|
    (gl-app-draw :draw)
    (gl-app-point :gl-app-point)
    (gl-app-line :gl-app-line)
    (gl-app-triangle :gl-app-triangle)
    (gl-app-triangle-strip :gl-app-triangle-strip)
    (gl-app-quad :gl-app-quad)
    (gl-app-quad-strip :gl-app-quad-strip)
    (gl-app-polygon :gl-app-polygon)
    (gl-app-push-matrix :push-matrix)
    (gl-app-pop-matrix :pop-matrix)
    (gl-app-load-identity :load-identity)
    (gl-app-set-matrix-mode :set-matrix-mode)
    (gl-app-projection-matrix :matrix-mode)
    (gl-app-modelview-matrix :matrix-mode)
    |#
    (gl-app-cube  1)
    (gl-app-ball 1)
    (gl-app-circle 1)
    (gl-app-cylinder 2)
    (gl-app-point-size 1)
    (gl-app-line-width 1)
    (gl-app-color 3)
    (gl-app-rotate 3)
    (gl-app-translate 3)
    (gl-app-scale 3)
    (gl-app-repeat-n 2)
    (gl-app-grid-repeat 3)
    (gl-app-set-var 2)
    (abs 1)
    (sin 1)
    (asin 1)
    (cos 1)
    (tan 1)
    (atan 1)
    (sqr 1)
    (+ 2)
    (- 2)
    (* 2)
    (/- 2)
    (mod 2)
    (div 2)))

;; OpenGL Free Drawing specific functions
;; ...

;;; OpenGL Free Drawing problem definition
(defclass entity-opengl-free-drawing-3d (entity-function)
  ())


(defmethod compiled-program ((o entity-opengl-free-drawing-3d))
  "Answer the compiled function for <o>."
  (compile nil `(lambda () ,(program o))))


;;; Environment auxiliars
(defmethod default-language ((o entity-opengl-free-drawing-3d))
  (system-get 'opengl-free-drawing-default-language))

(defmethod possible-languages ((o entity-opengl-free-drawing-3d))
  (list 
   (system-get 'opengl-free-drawing-default-language)))

(defmethod default-fitness-evaluators ((o entity-opengl-free-drawing-3d))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get 'opengl-free-drawing-default-evaluator)))

(defmethod drawablep ((o entity-opengl-free-drawing-3d))
  "Answer whether <o> can be displayed on the GUI."
  t)

(defmethod draw-in-pixmap (pinboard object pane (o entity-opengl-free-drawing-3d) parent-pinboard x y)
  "Draws object in the pixmap of pinboard interface."
  (not-available-pixmap pinboard object pane o parent-pinboard x y))

(defmethod draw-opengl-on ((o entity-opengl-free-drawing-3d) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (let* ((width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (expresion (compiled-program o))
         (ix (/ width 10))
         (iy (/ height 10))
         (cx (min width 100))
         (cy (min height 100))
         (dx (/ width cx))
         (dy (/ height cy)))
    (opengl:rendering-on (canvas)
      ;(initialize-ortho-2d 0.d0 width height 0.d0)
      (eval (program o)))
  (handler-case (opengl:swap-buffers canvas)
    (error (function) nil))))

;;; Fitness evaluator
(defmethod objective-class ((evaluator mux-fitness-evaluator))
  'entity-opengl-free-drawing-3d)

(defun evaluate-opengl-free-drawing-3d (evaluator object)
  "Evaluation method for OpenGL free drawing object."
  (let ((compiled-program (compiled-program object))
    (setf (fitness object) 1)
    1))


;;; Add system objects
(system-add
 ;; Grammars for OpenGL free drawing problems
 (make-instance 'context-free-grammar
                :name 'default-opengl-free-drawing-grammar
                :lexer 'lisp-math-expression-lexer
                :parser-initializer 'initialize-opengl-free-drawing-expression-parser-3d
                :productions (opengl-free-drawing-grammar-productions-3d)
                :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :4-ary-operator 
                                    :vertex-expression :expresion)))

(system-add
 ;; languages for OpenGL free drawing problems
 (make-instance 'cfg-tree-language 
                :name 'opengl-free-drawing-default-language
				:description "Free drawing language"
                :grammar (system-get-copy 'default-opengl-free-drawing-grammar)
                :simplification-patterns *opengl-free-drawing-editing-patterns*
                :functions (entity-opengl-free-drawing-default-functions-info-3d)
                :terminals '()
                :variables '()
                :tokens *opengl-free-drawing-expression-tokens-3d*
                :valid-new-expresion-function 'create-new-random-valid
                :operators (default-genetic-operators-probability-lisp-expression)))


#|
;; Circle sorrounded by a romboid
(gl-app-draw 
 (gl-app-sphere 1)
 (gl-app-translate 0 2)
 (gl-begin-lines
  (gl-app-rotate 0 0 90)
  (gl-app-vertex 0 0)
  (gl-app-rotate 0 0 90)
  (gl-app-vertex 0 0)
  (gl-app-rotate 0 0 90)
  (gl-app-vertex 0 0)
  (gl-app-rotate 0 0 90)
  (gl-app-vertex 0 0)))
|#
