
;; OpenGL primitive drawing language definition 
(defparameter *opengl-primitive-drawing-expression-tokens*
  '(;; Color modifier
    (gl-app-frag-color-4f :4-ary-operator)
    (gl-app-frag-color-3fv :3-ary-operator)
    ;; Texture modifier
    ;(gl-app-frag-tex-1f :3-ary-operator)
    ;(gl-app-frag-tex-3fv :3-ary-operator)
    ;; Common functions
    (ceil :1-ary-operator)
    (floor :1-ary-operator)
    (fract :1-ary-operator)
    ;; And and trigonometric functions
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (cos :1-ary-operator)
    (tan :1-ary-operator)
    (asin :1-ary-operator)
    (acos :1-ary-operator)
    (atan :1-ary-operator)
    (atan-2 :2-ary-operator)
    (sqr :1-ary-operator)
    (radians :1-ary-operator)
    (degrees :1-ary-operator)
    ;; Texture set
    (set-texture :set-texture)
    ;; Geometric
    (distance :2-ary-vertex-operator)
    (normalize :1-ary-vertex-operator)
    ;; Operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/- :2-ary-operator)))

#|
Selected subset:
---------------

Key:
vec = vec2 | vec3 | vec4 
mat = mat2 | mat3 | mat4 
ivec = ivec2 | ivec3 | ivec4 
bvec = bvec2 | bvec3 | bvec4 
genType = float | vec2 | vec3 | vec4 

Samplers are types representing textures. They are used for texture sampling. 
Sampler types have to be uniform. They are not allowed to be declared as a non-uniform type. 

sampler1D, sampler2D, sampler3D		1D, 2D and 3D texture
samplerCube				Cube Map texture
sampler1Dshadow, sampler2Dshadow	1D and 2D depth-component texture


Angle and Trigonometry Functions (8.1 p51)
------------------------------------------
genType  sin( genType ) 
genType  cos( genType ) 
genType  tan( genType ) 
genType  asin( genType ) 
genType  acos( genType ) 
genType  atan( genType, genType ) 
genType  atan( genType ) 
genType  radians( genType )
genType  degrees( genType ) 

Exponential functions
---------------------
T pow(T x, T y) x
T exp(T x)  e
T log(T x) ln
T exp2(T x)  2
T log2(T x) log
T sqrt(T x) square root
T inversesqrt(T x)  inverse square root


Common Functions (8.3 p52)
--------------------------
genType  abs( genType ) 
genType  ceil( genType ) 
genType  clamp( genType, genType, genType ) 
genType  clamp( genType, float, float ) 
genType  floor( genType ) 
genType  fract( genType ) 
genType  max( genType, genType ) 
genType  max( genType, float ) 
genType  min( genType, genType ) 
genType  min( genType, float ) 
genType  mix( genType, genType, genType ) 
genType  mix( genType, genType, float ) 
genType  mod( genType, genType ) 
genType  mod( genType, float ) 
genType  sign( genType ) 
genType  smoothstep( genType, genType, genType ) 
genType  smoothstep( float, float, genType ) 
genType  step( genType, genType ) 
genType  step( float, genType )

Geometric Functions (8.4 p54)
-----------------------------
vec4  ftransform()           Vertex ONLY
vec3  cross( vec3, vec3 ) 
float  distance( genType, genType ) 
float   dot( genType, genType ) 
genType  faceforward( genType V, genType I, genType N ) 
float   length( genType ) 
genType  normalize( genType ) 
genType  reflect( genType I, genType N ) 
genType  refract( genType I, genType N, float eta )

Texture Lookup Functions (8.7 p56)  
----------------------------------
Optional bias term is Fragment ONLY 
vec4  texture1D( sampler1D, float [,float bias] ) 
vec4  texture1DProj( sampler1D, vec2 [,float bias] ) 
vec4  texture1DProj( sampler1D, vec4 [,float bias] ) 
vec4  texture2D( sampler2D, vec2 [,float bias] ) 
vec4  texture2DProj( sampler2D, vec3 [,float bias] ) 
vec4  texture2DProj( sampler2D, vec4 [,float bias] ) 
vec4  texture3D( sampler3D, vec3 [,float bias] ) 
vec4  texture3DProj( sampler3D, vec4 [,float bias] ) 
vec4  textureCube( samplerCube, vec3 [,float bias] ) 
vec4  shadow1D( sampler1DShadow, vec3 [,float bias] ) 
vec4  shadow2D( sampler2DShadow, vec3 [,float bias] ) 
vec4  shadow1DProj( sampler1DShadow, vec4 [,float bias] ) 
vec4  shadow2DProj( sampler2DShadow, vec4 [,float bias] )

Noise Functions (8.9 p60)
-------------------------
float  noise1( genType ) 
vec2  noise2( genType ) 
vec3  noise3( genType ) 
vec4  noise4( genType )

FRAGMENT SHADER VARIABLES
-------------------------

Special Output Variables (7.2 p43)  access=RW
---------------------------------------------

Inputs:
    in vec4 gl_FragCoord; 
    in bool gl_FrontFacing; 
    in float gl_ClipDistance[]; 
    in vec2 gl_PointCoord; 
    in int gl_PrimitiveID;  
    in int gl_SampleID; 
    in vec2 gl_SamplePosition;
    in int gl_SampleMask[];
    in gl_PerFragment {
    in float gl_FogFragCoord; 
    in vec4 gl_TexCoord[];  
    in vec4 gl_Color;  
    in vec4 gl_SecondaryColor;

Varying Inputs (7.6 p48)  access=RO
-----------------------------------
varying vec4  gl_Color; 
varying vec4  gl_SecondaryColor; 
varying vec4  gl_TexCoord[ ];        MAX=gl_MaxTextureCoords
varying float  gl_FogFragCoord;

Special Input Variables (7.2 p43)  access=RO
--------------------------------------------
vec4  gl_FragCoord;                pixel coordinates
bool  gl_FrontFacing;


BUILT-IN CONSTANTS (7.4 p44)
----------------------------
const int gl_MaxVertexUniformComponents;  
const int gl_MaxFragmentUniformComponents; 
const int gl_MaxVertexAttribs;  
const int gl_MaxVaryingFloats;  
const int gl_MaxDrawBuffers;  
const int gl_MaxTextureCoords;  
const int gl_MaxTextureUnits; 
const int gl_MaxTextureImageUnits;  
const int gl_MaxVertexTextureImageUnits; 
const int gl_MaxCombinedTextureImageUnits; 
const int gl_MaxLights; 
const int gl_MaxClipPlanes;

VECTOR COMPONENTS (5.5 p 30)
----------------------------

component names may not be mixed across sets  
--------------------------------------------
        x, y, z, w 
        r, g, b, a 
        s, t, p, q 

Aggregate Operations and Constructors
-------------------------------------
Matrix Constructor Examples [5.4]
---------------------------------
mat2(vec2, vec2);  // one column per argument
mat3x2(vec2, vec2, vec2); // column 1
mat2(float, float, float, float);  // column 2
mat2x3(vec2, float, vec2, float); // column 2
mat4x4(mat3x3);  // mat3x3 to upper left, set lower right to 1, fill rest with zero


Blending [4.1.7] [4.1.8]
------------------------
Enable/Disable(BLEND) 
Enablei/Disablei(BLEND, uint index)
void BlendEquation(enum mode);
void BlendEquationi(uint buf, enum mode);
void BlendEquationSeparate(enum modeRGB, enum modeAlpha);
void BlendEquationSeparatei(uint buf, enum modeRGB, enum modeAlpha);
void BlendFunc(enum src, enum dst);
void BlendFunci(uint buf, enum src, enum dst);
void BlendFuncSeparate(enum srcRGB, enum dstRGB, enum srcAlpha, enum dstAlpha);
void BlendFuncSeparatei(uint buf, enum srcRGB, enum dstRGB, enum srcAlpha, enum dstAlpha);


|#



(defun initialize-opengl-primitive-drawing-expression-parser (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start :open :draw view-set draw-expression-list :close)
                `(,$3 ,$4 ,$5))
               ((view-set :open :view-set-keyword :view-setter :close)
                `(:view-set (,$2 ,$3)))
               ;; Draw operation list
               ((draw-operation-list :open draw-operation :close)
                `(:expresion (,$2)))
               ((draw-operation-list :open draw-operation draw-operation-list :close)
                `(:expresion (,$2 ,$3)))
               ;; Operations
               ((expresion :open :1-ary-operator expresion :close)
                `(:expresion (,$2 ,$3)))
               ((expresion :open :2-ary-operator expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4)))
               ((expresion :open :3-ary-operator expresion expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5)))
               ((expresion :open :4-ary-operator expresion expresion expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5 ,$6)))
               ;; Vertex operations
               ((vertex-expression expresion expresion expresion)
                `(:vertex-expression ,$1 ,$2 ,$3))
               ((vertex-expression :open :1-ary-vertex-operator vertex-expression :close)
                `(vertex-expression ,$2 ,$3))
               ((vertex-expression :open :2-ary-vertex-operator vertex-expression vertex-expression :close)
                `(vertex-expression ,$2 ,$3 ,$4))
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
               ((draw-operation mat2(float, float, float, float);  // column 2
                 :open :3-ary-primitive-operator vertex-expresion vertex-expresion vertex-expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5)))
               ((draw-operation 
                 :open 
                 :4-ary-primitive-operator vertex-expresion vertex-expresion vertex-expresion vertex-expresion 
                 :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5 ,$6)))
               ;; Draw operations: primitives list
               ;; ...
               ;; Var, vertex and number
               ((expresion :var)
                `(:expresion ,$1))
               ((expresion :number)
                `(:expresion ,$1)))))


(defun opengl-primitive-drawing-grammar-productions ()
  '((start :open :draw view-set expresion :close)
    (view-set :open :view-set-keyword :view-setter :close)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion :open 4-ary-operator expresion expresion expresion expresion :close)
    (expresion var)
    (var :var)
    (var :number)))


(defun entity-opengl-primitive-drawing-default-functions-info ()
  '((int-and 2) (int-or 2) (int-not 1) (int-ife 3)))


(defvar *opengl-free-drawing-editing-patterns*
  nil)


;; OpenGL Free Drawing specific functions
(defun int-and (a b)
  (declare (fixnum a) (fixnum b))
  (if (and (= a 1) (= b 1))
      1
    0))

;;; ...

;;; OpenGL Free Drawing problem definition
(defclass entity-opengl-primitive-drawing (entity-function)
  ())


(defmethod compiled-program ((o entity-opengl-primitive-drawing))
  "Answer the compiled function for <o>."
  (compile nil `(lambda () ,(program o))))


;;; Environment auxiliars
(defmethod default-language ((o entity-opengl-primitive-drawing))
  (system-get 'opengl-free-drawing-default-language))

(defmethod possible-languages ((o entity-opengl-primitive-drawing))
  (list 
   (system-get 'opengl-free-drawing-default-language)))

(defmethod default-fitness-evaluators ((o entity-opengl-primitive-drawing))
  "Answer the default classes that can evaluate object fitness."
  (list 
   (system-get 'opengl-free-drawing-default-evaluator)))

(defmethod drawablep ((o entity-opengl-primitive-drawing))
  "Answer whether <o> can be displayed on the GUI."
  t)


;;; Fitness evaluator
(defclass opengl-primitive-drawing-evaluator (entity-evaluator)
  ((fitness-function :initarg :fitness-function :accessor fitness-function)))


(defmethod evaluate ((evaluator entity-opengl-primitive-drawing) (object opengl-primitive-drawing-evaluator))
  "Use <evaluator> to calculate and answer <object> fitness."
  (funcall (fitness-function evaluator) evaluator object))

(defmethod objective-class ((evaluator entity-opengl-primitive-drawing))
  'entity-opengl-free-drawing)

(defun evaluate-opengl-primitive-drawing (evaluator object)
  "Evaluation method for OpenGL free drawing object."
  (let ((compiled-program (compiled-program object))
    ;; ...
    (setf (fitness object) 1)
    1))


;;; Add system objects
(system-add
 ;; Grammars for OpenGL free drawing problems
 (make-instance 'context-free-grammar
                :name 'default-opengl-primitive-grammar
                :lexer 'lisp-math-expression-lexer
                :parser-initializer 'initialize-mux-expression-parser
                :productions (mux-grammar-productions)
                :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :4-ary-operator :expresion)))

(system-add
 ;; Languages for OpenGL free drawing problems
 (make-instance 'cfg-tree-language 
                :name 'opengl-primitive-drawing-default-language
                :grammar (system-get-copy 'default-mux-grammar)
                :simplification-patterns *opengl-primitive-drawing-editing-patterns*
                :functions (entity-opengl-primitive-drawing-default-functions-info)
                :terminals nil
                :variables nil
                :tokens *mux-expression-tokens*
                :valid-new-expresion-function 'create-new-random-valid
                :simplification-function 'simplify
                :operators (default-genetic-operators-probability-lisp-expression))
  ;; Mux problems fitness evaluators
  (make-instance 'opengl-primitive-drawing-evaluator
                 :name 'opengl-primitive-drawing-default-evaluator
                 :description "OpenGL free drawing default evaluator"
                 :fitness-function 'evaluate-opengl-primitive-drawing
                 :min-fitness 0
                 :max-fitness 10
                 :solution-fitness 9.8))


#|
(draw 
 (view-set-keyword :view-setter) 
 (draw-expression-list))
|#
