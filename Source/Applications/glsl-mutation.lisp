(defun get-productions ()
  (mapcar (lambda (o) (car o))
          (append 
           (list '((START EXPTHREE) $1)
                 ;; Hook with return type
                 '((EXP EXPTHREE) (SYSTEM::BQ-LIST :EXP $1)))
           ;; Function expansions
           (glsl-grammar-productions *glsl-exp-structure-data*)
           ;; Operator expansions
           ;; #TODO:
           (list '((EXPONE VAR1) (SYSTEM::BQ-LIST :EXPONE $1))
                 '((EXPTWO VAR2) (SYSTEM::BQ-LIST :EXPTWO $1))
                 '((EXPTHREE VAR3) (SYSTEM::BQ-LIST :EXPTHREE $1))
                 '((EXPFOUR VAR4) (SYSTEM::BQ-LIST :EXPFOUR $1))   
                 '((VAR1 :VAR1) (SYSTEM::BQ-LIST :VAR1 $1))
                 '((VAR2 :VAR2) (SYSTEM::BQ-LIST :VAR2 $1))
                 '((VAR3 :VAR3) (SYSTEM::BQ-LIST :VAR3 $1))
                 '((VAR4 :VAR4) (SYSTEM::BQ-LIST :VAR4 $1))))))

(defun glsl-grammar-productions (definition)
  (let ((productions))
    (dolist (i definition)
      (let ((spec (car i))
            (possible (cdr i)))
        (dolist (j possible)
          (appendf productions (list (productions-from-line i j))))))
    productions))
				
(defun productions-from-line (i j)
  "Add expanded productions for <i>, <j> into <productions>."
  (let* ((splitted (split-sequence "-" (symbol-name (car i))))
         (return-type (car splitted))
         (arguments (mapcar (lambda (o) (get-exp-type-from-argument-string o)) (cdr splitted)))
         (production (expand-production return-type j arguments))
         (generated (expand-generated return-type arguments)))
    (append (list production) (list generated))))

(defun expand-production (return-type function-name arguments)
  "Answer a list with production description values for <spec>."
  (append (list (get-exp-type-from-argument-string return-type)
                :open
                (intern function-name :keyword))
          arguments 
          (list :close)))

(defun expand-generated (return-type arguments)
  "Answer expression to build parse tree when parsing for production <i>."
  (list 'SYSTEM::BQ-LIST 
        (get-exp-type-from-argument-string return-type)
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
        'expone
      (if (equals value "f2")
          'exptwo
        (if (equals value "f3")
            'expthree
          (if (equals value "f4")
              'expfour
            ;; #TODO: Throw error
            nil))))))

(defvar *glsl-exp-structure-data*
  '(;; float functions
    (f1-f1 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX FWIDTH)
    (f1-f1-f1 ATAN POW MIN MAX MOD STEP DISTANCE DOT REFLECT)
    (f1-f1-f1-f1 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f1-f2-f2 DISTANCE DOT)
    (f1-f3-f3 DISTANCE DOT)
    (f1-f4-f4 DISTANCE DOT)
    ;; vec2 functions
    (f2-f1-f1 VEC2)
    (f2-f2 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX FWIDTH)
    (f2-f2-f2 ATAN POW MIN MAX MOD STEP REFLECT)
    (f2-f2-f1 MIN MAX MOD)
    (f2-f2-f1-f1 CLAMP)
    (f2-f2-f2-f2 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f2-f2-f2-f1 MIX REFLECT)
    (f2-f1-f1-f2 SMOOTHSTEP)
    (f2-f1-f2 STEP)
    ;; vec3 functions
    (f3-f1-f1-f1 VEC3)
    (f3-f3 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX FWIDTH)
    (f3-f3-f3 ATAN POW MIN MAX MOD STEP REFLECT CROSS)
    (f3-f3-f1 MIN MAX MOD)
    (f3-f3-f1-f1 CLAMP)
    (f3-f3-f3-f3 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f3-f3-f3-f1 MIX REFLECT)
    (f3-f1-f1-f3 SMOOTHSTEP)
    (f3-f1-f3 STEP)
    ;; vec4 functions		
    (f4-f1-f1-f1-f1 VEC4)
    (f4-f4 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX FWIDTH)
    (f4-f4-f4 ATAN POW MIN MAX MOD STEP REFLECT)
    (f4-f4-f1 MIN MAX MOD)
    (f4-f4-f1-f1 CLAMP)
    (f4-f4-f4-f4 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f4-f4-f4-f1 MIX REFLECT)
    (f4-f1-f1-f4 SMOOTHSTEP)
    (f4-f1-f4 STEP)))

(defun test-language-glsl (vars)
  (let ((language (create-language-from (gensym) vars)))
    (parse (grammar language) "(SIN (VEC3 (SIN 0.0) 0.0 1.0))")))


(defun create-language-from (name vars)
  (let ((grammar (make-instance 'context-free-grammar
                                :name (intern (format nil "grammar-~a" name))
                                :lexer 'glsl-expressions-subset-lexer
                                :parser-initializer 'initialize-glsl-expressions-subset-parser-vec3
                                :productions (get-productions)
                                :crossover-nodes '(:expone :exptwo :expthree :expfour))))
    (system-add grammar)
    (make-instance 'cfg-tree-language 
                   :name (format nil "language-~a" name)
                   :description (format nil "language-~a" name)
                   :grammar (system-get (intern (format nil "grammar-~a" name)))
                   :simplification-patterns nil
                   :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                   :functions *glsl-exp-structure-data*
                   :variables vars
                   :terminals (append vars '(:constant))
                   :tokens (create-tokens-for-language *glsl-exp-structure-data*)
                   :valid-new-expresion-function 'create-new-random-valid
                   :simplification-function nil
                   :operators (default-genetic-operators-probability-polynomial-expression))))

(defun create-tokens-for-language (spec)
  (let ((result (make-hash-table)))
    (dolist (i spec)
      (dolist (j (cdr i))
        (setf (gethash j result) t)))
    (mapcar (lambda (o) (list o (intern o :keyword)))
            (keys result))))
    

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
   (append 
    (list 'DEFPARSER 
          (QUOTE GLSL-GRAMMAR)
          '((START EXP3) $1)
          ;; Hook with return type
          '((EXP VAR3) (SYSTEM::BQ-LIST :EXP $1)))
    ;; Function expansions
    (glsl-grammar-productions *glsl-exp-structure-data*)
    ;; Operator expansions
    ;; #TODO:
    (list '((EXPONE VAR1) (SYSTEM::BQ-LIST :EXPONE $1))
          '((EXPTWO VAR2) (SYSTEM::BQ-LIST :EXPTWO $1))
          '((EXPTHREE VAR3) (SYSTEM::BQ-LIST :EXPTHREE $1))
          '((EXPFOUR VAR4) (SYSTEM::BQ-LIST :EXPFOUR $1))   
          '((VAR1 :VAR1) (SYSTEM::BQ-LIST :VAR1 $1))
          '((VAR2 :VAR2) (SYSTEM::BQ-LIST :VAR2 $1))
          '((VAR3 :VAR3) (SYSTEM::BQ-LIST :VAR3 $1))
          '((VAR4 :VAR4) (SYSTEM::BQ-LIST :VAR4 $1))))))

#|
;;
;; ((EXP4 :OPEN f4-f4-f4 EXP4 EXP4 :CLOSE) 
;;  (SYSTEM::BQ-LIST :EXP4 (SYSTEM::BQ-LIST $2 $3)))
;;

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
(create-language-from 'glsl-example-language '(x))
(glsl-grammar-productions *glsl-exp-structure-data*)

|#


(defun minimum-production-size (grammar all-productions production &optional passed)
  (if (and (symbolp production) (keywordp production))
      (if (structural-symbol production) 0 1)  
    (let (;(p (mapcar 
          ;    (lambda (value) (car value))
          ;    (non-recursive-productions-for all-productions production)))
          (q (non-recursive-right-productions-for all-productions (append (list production) passed)))
          (minimum-size *infinite-productions-size-value*))
      (dolist (i q)
        (let ((local-size 0))
          (dolist (j (cdr i))
            (incf local-size (minimum-production-size grammar all-productions j (append (list j) passed))))
          (if (or (null minimum-size)
                  (< local-size minimum-size))
              (setf minimum-size local-size))))
      minimum-size)))

