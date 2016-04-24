
(defun glsl-grammar-productions (definition)
  (let ((productions))
    (dolist (i definition)
      (let ((spec (car i))
            (possible (cdr i)))
        (dolist (j possible)
          (appendf productions (productions-from-line i j)))))
    productions))
				
(defun productions-from-line (i j)
  "Add expanded productions for <i>, <j> into <productions>."
  (let* ((splitted (split-sequence "-" (symbol-name (car i))))
         (return-type (car splitted))
         (arguments (mapcar (lambda (o) (get-var-type-from-argument-string o)) (cdr splitted)))
         (production (expand-production i splitted))
         (generated (expand-generated i arguments splitted)))
    (append (list production)
            (list generated))))

;;
;; ((EXP4 :OPEN f4-f4-f4 EXP4 EXP4 :CLOSE) 
;;  (SYSTEM::BQ-LIST :EXP4 (SYSTEM::BQ-LIST $2 $3)))
;;

(defun expand-production (i splitted)
  "Answer a list with production description values for <spec>."
  (let ((return-type (car splitted))
        (arguments (mapcar (lambda (o) (get-var-type-from-argument-string o)) (cdr splitted))))
    (append (list (get-exp-type-from-argument-string return-type)
                  :open 
                  (intern (car i) :keyword))
            arguments 
            (list :close))))

(defun expand-generated (i arguments splitted)
  "Answer expression to build parse tree when parsing for production <i>."
  (list 'SYSTEM::BQ-LIST 
        (intern (get-exp-type-from-argument-string (car splitted)) :keyword)
        (append (list 'SYSTEM::BQ-LIST)
                (loop for i from 1 to (1+ (length arguments)) collect 
                      (argument-index-symbol i)))))
  
(defun argument-index-symbol (index)
  (ecase index
    ((1) '$1)
    ((2) '$2)
    ((3) '$3)
    ((4) '$4)
    ((5) '$5)
    ((6) '$6)
    ;; #ERROR:
    ('otherwise nil)))

(defun get-exp-type-from-argument-string (o)
  (let ((value (string-downcase o)))
    (if (equals value "f1")
        'exp1
      (if (equals value "f2")
          'exp2
        (if (equals value "f3")
            'exp3
          (if (equals value "f4")
              'exp4
            ;; #TODO: Throw error
            nil))))))

(defun get-var-type-from-argument-string (o)
  (let ((value (string-downcase o)))
    (if (equals value "f1")
        :exp1
      (if (equals value "f2")
          :exp2
        (if (equals value "f3")
            :exp3
          (if (equals value "f4")
              :exp4
            ;; #TODO: Throw error
            nil))))))

(defvar *glsl-exp-structure-data*
  '(;; float functions
    (f1-f1 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX GENTYPE FWIDTH)
    (f1-f1-f1 ATAN POW MIN MAX MOD STEP DISTANCE DOT REFLECT)
    (f1-f1-f1-f1 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f1-f2-f2 DISTANCE DOT)
    (f1-f3-f3 DISTANCE DOT)
    (f1-f4-f4 DISTANCE DOT)
    ;; vec2 functions
    (f2-f1-f1 VEC2)
    (f2-f2 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX GENTYPE FWIDTH)
    (f2-f2-f2 ATAN POW MIN MAX MOD STEP REFLECT)
    (f2-f2-f1 MIN MAX MOD)
    (f2-f2-f1-f1 CLAMP)
    (f2-f2-f2-f2 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f2-f2-f2-f1 MIX REFLECT)
    (f2-f1-f1-f2 SMOOTHSTEP)
    (f2-f1-f2 STEP)
    ;; vec3 functions
    (f3-f1-f1-f1 VEC3)
    (f3-f3 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX GENTYPE FWIDTH)
    (f3-f3-f3 ATAN POW MIN MAX MOD STEP REFLECT CROSS)
    (f3-f3-f1 MIN MAX MOD)
    (f3-f3-f1-f1 CLAMP)
    (f3-f3-f3-f3 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f3-f3-f3-f1 MIX REFLECT)
    (f3-f1-f1-f3 SMOOTHSTEP)
    (f3-f1-f3 STEP)
    ;; vec4 functions		
    (f4-f1-f1-f1-f1 VEC4)
    (f4-f4 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX GENTYPE FWIDTH)
    (f4-f4-f4 ATAN POW MIN MAX MOD STEP REFLECT)
    (f4-f4-f1 MIN MAX MOD)
    (f4-f4-f1-f1 CLAMP)
    (f4-f4-f4-f4 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f4-f4-f4-f1 MIX REFLECT)
    (f4-f1-f1-f4 SMOOTHSTEP)
    (f4-f1-f4 STEP)
    ))

(defun test-language-glsl (vars)
  (let ((language (create-language-from (gensym) vars (entity-function-default-functions-info))))
    (parse (grammar language) "nil")))


(defun create-language-from (name vars functions)
  (let ((grammar (make-instance 'context-free-grammar
                                :name 'glsl-exp-subset-grammar
                                :lexer 'glsl-expressions-subset-lexer
                                :parser-initializer 'initialize-glsl-expressions-subset-parser-vec3
                                :productions (glsl-grammar-productions *glsl-exp-structure-data*)
                                :crossover-nodes '(:exp1 :exp2 :exp3 :exp4))))
    (system-add grammar)
    (make-instance 'cfg-tree-language 
                   :name name
                   :description (format nil "F(n) CFG ~a" name)
                   :grammar (system-get-copy 'glsl-exp-subset-grammar)
                   :simplification-patterns nil
                   :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                   :functions *glsl-exp-structure-data*
                   ;; #TODO: Feed with vars
                   :variables vars
                   ;; #TODO: Feed with vars too 
                   :terminals (append vars '(:constant))
                   ;; #TODO: Feed with functions
                   :tokens (create-tokens-for-language *glsl-expressions-subset-tokens-base*)
                   :valid-new-expresion-function 'create-new-random-valid
                   :simplification-function nil
                   :operators (default-genetic-operators-probability-lisp-expression))))

;; #TODO:
(defun create-tokens-for-language ()
  (let ((result
         (append vars)))))

(defun glsl-expressions-subset-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (glsl-expressions-subset-get-token grammar symbol)
      nil)))

(defun glsl-expressions-subset-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (when (equal token-type :unknown)
        (if (numberp word) 
            (setf token-type :constant))
        (setf token-type :constant))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))


(defun initialize-glsl-expressions-subset-parser-vec3 (name)
  (eval
   (let ((name 'sample-grammar))
     (append 
      (list 'DEFPARSER 
            '(QUOTE GLSL-GRAMMAR)
            '((START EXP3) $1)
            ;; Hook with return type
            '((EXP VAR3) (SYSTEM::BQ-LIST :EXP $1)))
      ;; Function expansions
      (glsl-grammar-productions *glsl-exp-structure-data*)
      ;; Operator expansions
      ;; #TODO:
      (list '((EXP1 VAR1) (SYSTEM::BQ-LIST :EXP $1))
            '((EXP2 VAR2) (SYSTEM::BQ-LIST :EXP $1))
            '((EXP3 VAR3) (SYSTEM::BQ-LIST :EXP $1))
            '((EXP4 VAR4) (SYSTEM::BQ-LIST :EXP $1))   
            '((VAR1 :F1) (SYSTEM::BQ-LIST :VAR $1))
            '((VAR2 :F2) (SYSTEM::BQ-LIST :VAR $1))
            '((VAR3 :F3) (SYSTEM::BQ-LIST :VAR $1))
            '((VAR4 :F4) (SYSTEM::BQ-LIST :VAR $1)))))))

#|
     '(DEFPARSER (QUOTE GLSL-GRAMMAR)
                 ((START EXP3) $1)
                 ;; Hook with return type
                 ((EXP VAR3) (SYSTEM::BQ-LIST :EXP $1))
                 ;; Function expansions
                 placeholder-functions
                 placeholder-operators
                 ((EXP1 VAR1) (SYSTEM::BQ-LIST :EXP $1))
                 ((EXP2 VAR2) (SYSTEM::BQ-LIST :EXP $1))
                 ((EXP3 VAR3) (SYSTEM::BQ-LIST :EXP $1))
                 ((EXP4 VAR4) (SYSTEM::BQ-LIST :EXP $1))   
                 ((VAR1 :F1) (SYSTEM::BQ-LIST :VAR $1))
                 ((VAR2 :F2) (SYSTEM::BQ-LIST :VAR $1))
                 ((VAR3 :F3) (SYSTEM::BQ-LIST :VAR $1))
                 ((VAR4 :F4) (SYSTEM::BQ-LIST :VAR $1)))))))
|#

              ;;((EXP :OPEN :1-ARY-OPERATOR EXP :CLOSE) 
              ;; (SYSTEM::BQ-LIST :EXP (SYSTEM::BQ-LIST $2 $3)))
              ;;((EXP :OPEN :2-ARY-OPERATOR EXP EXP :CLOSE)
              ;; (SYSTEM::BQ-LIST :EXP (SYSTEM::BQ-LIST $2 $3 $4))) 
              ;;((EXP :OPEN :3-ARY-OPERATOR EXP EXP EXP :CLOSE) 
              ;; (SYSTEM::BQ-LIST :EXP (SYSTEM::BQ-LIST $2 $3 $4 $5))) 


#|

(defun entity-function-default-functions-info ()
  '((+ 2) (- 2) (* 2) (/ 2)
    (sin 1) (cos 1) (sqr 1) (tan) (abs 1) (atan) (acos) (asin) 
    (pow 1) (min 2) (max 2) (clamp 3) (floor 1) (fract 1) (smoothstep 3)
    (mod 2) (exp2 1) (log2 1) (sqr 1) 
    (sign) (inversesqrt) (radians) (degrees) (normalize)
    (cross) (dot) (distance) (reflect) (refract) 
    ;; Ver si sumar
    (noise1) (noise2) (noise3) (noise4)))

(defun initialize-glsl-expressions-subset-parser (name)
  (eval
   `(defparser ,name
               ((start exp)
                $1)
               ((exp :open :1-ary-operator exp :close)
                `(:exp (,$2 ,$3)))
               ((exp :open :2-ary-operator exp exp :close)
                `(:exp (,$2 ,$3 ,$4)))
               ((exp :open :3-ary-operator exp exp exp :close)
                `(:exp (,$2 ,$3 ,$4 ,$5)))
               ((exp vector)
                `(:exp ,$1))
               ((vector :open :create-vector scalar scalar scalar :close)
                `(:vector (,$2 ,$3 ,$4 ,$5)))
               ((scalar constant)
                `(:scalar ,$1))
               ((scalar var1)
                `(:scalar ,$1))
               ((constant :constant)
                `(:constant ,$1))
               ((var1 :F1)
                `(:var ,$1))
               ((var2 :F2)
                `(:var ,$1))
               ((var3 :F3)
                `(:var ,$1))
               ((var4 :F4)
                `(:var ,$1))
               ;; Combinations (#TODO: not all are used, delete unusefull ones)
               )
   ))

(defun glsl-expressions-subset-grammar-productions ()
  '((start exp)
    ;; Expansión por arity
    (exp :open :1-ary-operator exp :close)
    (exp :open :2-ary-operator exp exp :close)
    (exp :open :3-ary-operator exp exp exp :close)
    (constant :constant)
    (var1 :F1)
    (var2 :F2)
    (var3 :F3)
    (var4 :F4)))

(test-language-glsl '("x.rgb" "x.xy" "g.r" "x"))

|#
