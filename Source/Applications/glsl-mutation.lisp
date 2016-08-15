
(defun glsl-grammar-productions (definition)
  (let ((productions))
    (dolist (i definition)
      (let ((spec (car i))
            (possible (cdr i)))
        (dolist (j possible)
          (appendf productions (list (productions-from-line i j))))))
    productions))

(defun glsl-grammar-functions (definition)
  (let ((functions))
    (dolist (i definition)
      (let ((spec (car i))
            (possible (cdr i)))
        (dolist (j possible)
          (appendf functions (list j)))))
    (mapcar (lambda (o) (list o o)) (unique-eql functions))))
				
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
                ;; #TODO: Check if remove as a keyword and add intermediate productions, causing:
                ;;   - Refine crossover options 
                ;;   - 
                ;(intern function-name :keyword)
                (intern function-name)
                )
          arguments 
          (list :close)))

(defun expand-generated (return-type arguments)
  "Answer expression to build parse tree when parsing for production <i>."
  (list 'SYSTEM::BQ-LIST 
        (list 'quote (get-exp-type-from-argument-string return-type))
        (append (list 'SYSTEM::BQ-LIST)
                ;; #NOTE: index 2 is because :open symbol
                (loop for i from 2 to (+ 2 (length arguments)) collect 
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

;; #TODO: check assignment operators
;; *= += /= = 

(setf *glsl-exp-structure-data*
  '(;; float functions
    (f1-f1 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN LENGTH NORMALIZE DFDX DFDY FWIDTH)
    (f1-f1-f1 ATAN POW MIN MAX MOD STEP DISTANCE DOT REFLECT + - * /)
    (f1-f1-f1-f1 CLAMP MIX SMOOTHSTEP FACEFORWARD REFRACT)
    (f1-f2-f2 DISTANCE DOT)
    (f1-f3-f3 DISTANCE DOT)
    (f1-f4-f4 DISTANCE DOT)
    (f1-f2 LENGTH)
    (f1-f3 LENGTH)
    (f1-f4 LENGTH)
    ;; vec2 functions
    (f2-f1 VEC2)
    (f2-f1-f1 VEC2)
    (f2-f2 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN NORMALIZE DFDX DFDY FWIDTH)
    (f2-f2-f2 ATAN POW MIN MAX MOD STEP REFLECT + - * /)
    (f2-f2-f1 MIN MAX MOD + - * /)
    (f2-f2-f1-f1 CLAMP)
    (f2-f2-f2-f2 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f2-f2-f2-f1 MIX REFRACT)
    (f2-f1-f1-f2 SMOOTHSTEP)
    (f2-f1-f2 STEP + - * /)
    ;; vec3 functions
    (f3-f1 VEC3)
    (f3-f1-f2 VEC3)
    (f3-f2-f1 VEC3)
    (f3-f1-f1-f1 VEC3)
    (f3-f3 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN NORMALIZE DFDX DFDY FWIDTH)
    (f3-f3-f3 ATAN POW MIN MAX MOD STEP REFLECT CROSS + - * /)
    (f3-f3-f1 MIN MAX MOD + - * /)
    (f3-f3-f1-f1 CLAMP)
    (f3-f3-f3-f3 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f3-f3-f3-f1 MIX REFRACT)
    (f3-f1-f1-f3 SMOOTHSTEP)
    (f3-f1-f3 STEP + - * /)
    ;; vec4 functions		
    (f4-f1-f1-f1-f1 VEC4)
    (f4-f1-f3 VEC4)
    (f4-f3-f1 VEC4)
    (f4-f2-f2 VEC4)
    (f4-f1 VEC4)
    (f4-f1-f2-f1 VEC4)
    (f4-f2-f1-f1 VEC4)
    (f4-f1-f1-f2 VEC4)
    (f4-f4 SIN COS TAN ASIN ACOS ATAN RADIANS DEGREES EXP LOG EXP2 SQRT INVERSESQRT ABS CEIL FLOOR FRACT SIGN NORMALIZE DFDX DFDY FWIDTH)
    (f4-f4-f4 ATAN POW MIN MAX MOD STEP REFLECT + - * /)
    (f4-f4-f1 MIN MAX MOD + - * /)
    (f4-f4-f1-f1 CLAMP)
    (f4-f4-f4-f4 CLAMP MIX SMOOTHSTEP FACEFORWARD)
    (f4-f4-f4-f1 MIX REFRACT)
    (f4-f1-f1-f4 SMOOTHSTEP)
    (f4-f1-f4 STEP + - * /)))


(defun create-language-from (name vars)
  (let* ((tokens (create-tokens-for-language *glsl-exp-structure-data*))
         (grammar (make-instance 'glsl-grammar
                                 :name 'glsl-grammar-test
                                 :lexer 'glsl-expressions-subset-lexer
                                 ;; #NOTE: nil to generate parser description from updated-productions
                                 :parser-initializer nil
                                 ;; #NOTE: :productions should be deprecated when :definition is provided as argument
                                 :definition (glsl-parser-definition)
                                 :productions (glsl-productions)
                                 :crossover-nodes '(:expone :exptwo :expthree :expfour)))
         (language (make-instance 'glsl-language
                                  :name (format nil "language-~a" name)
                                  :description (format nil "language-~a" name)
                                  :grammar grammar
                                  :simplification-patterns nil
                                  :constants-strategy (system-get-copy 'default-ephemeral-0-10d)
                                  :functions (glsl-grammar-functions *glsl-exp-structure-data*)
                                  :variables vars
                                  :terminals (append (list :float) vars)
                                  :tokens tokens
                                  :valid-new-expresion-function 'create-new-random-valid
                                  :simplification-function nil
                                  :operators (default-genetic-operators-probability-polynomial-expression))))
    (setf (minimum-production-sizes (grammar language)) (glsl-minimum-production-sizes vars))
    language))

(defun create-tokens-for-language (spec)
  (let ((result (make-hash-table)))
    (dolist (i spec)
      (dolist (j (cdr i))
        (setf (gethash j result) t)))
    (mapcar (lambda (o) (list o  (intern o :keyword) #| o |#))
            (keys result))))

(defun glsl-expressions-subset-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol 
        (glsl-expressions-subset-get-token grammar symbol)
      nil))) 

(defun glsl-expressions-subset-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (when (equal token-type :unknown)
        (if (numberp word) 
            (setf token-type :float)))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun glsl-parser-definition ()
  (append 
   (list '((START EXPS) $1)
         ;; Hook with return type
         '((EXPS EXPFOUR) (SYSTEM::BQ-LIST :EXPS $1))
         '((EXPS EXPTHREE) (SYSTEM::BQ-LIST :EXPS $1))
         '((EXPS EXPTWO) (SYSTEM::BQ-LIST :EXPS $1))
         '((EXPS EXPONE) (SYSTEM::BQ-LIST :EXPS $1)))
   ;; Function expansions
   (glsl-grammar-productions *glsl-exp-structure-data*)
   ;; Variables
   (list '((EXPONE VAR1) (SYSTEM::BQ-LIST :EXPONE $1))
         '((EXPTWO VAR2) (SYSTEM::BQ-LIST :EXPTWO $1))
         '((EXPTHREE VAR3) (SYSTEM::BQ-LIST :EXPTHREE $1))
         '((EXPFOUR VAR4) (SYSTEM::BQ-LIST :EXPFOUR $1))
         '((EXPONE :FLOAT) (SYSTEM::BQ-LIST :EXPONE $1))
         '((VAR1 :VAR1) (SYSTEM::BQ-LIST :VAR1 $1))
         '((VAR2 :VAR2) (SYSTEM::BQ-LIST :VAR2 $1))
         '((VAR3 :VAR3) (SYSTEM::BQ-LIST :VAR3 $1))
         '((VAR4 :VAR4) (SYSTEM::BQ-LIST :VAR4 $1)))))

(defun glsl-productions ()
  (mapcar (lambda (o) (car o))
          (glsl-parser-definition)))

;(defun initialize-parser-from-definition (name o) 
;  (eval
;   (append (list 'defparser name)
;           (updated-definition (grammar o) (tokens o) (functions o) (variables o) (specialized-tokens o)))))

;; TODO: due to error when using above functions, broke process in two parts
(defun initialize-parser-from-definition (name o) 
  (eval
   (append (list 'defparser name)
'(((START EXPS) $1) 
  ((EXPS EXPFOUR) (SYSTEM::BQ-LIST :EXPS $1)) 
  ((EXPS EXPTHREE) (SYSTEM::BQ-LIST :EXPS $1)) 
  ((EXPS EXPTWO) (SYSTEM::BQ-LIST :EXPS $1)) 
  ((EXPS EXPONE) (SYSTEM::BQ-LIST :EXPS $1)) 
  ((EXPONE :OPEN SIN EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN COS EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN TAN EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN ASIN EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN ACOS EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN ATAN EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN RADIANS EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN DEGREES EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN EXP EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN LOG EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN EXP2 EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN SQRT EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN INVERSESQRT EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN ABS EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN CEIL EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN FLOOR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN FRACT EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN SIGN EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN LENGTH EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN NORMALIZE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN DFDX EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN DFDY EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN FWIDTH EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPONE :OPEN ATAN EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN POW EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN MIN EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN MAX EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN MOD EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN STEP EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DISTANCE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DOT EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN REFLECT EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN + EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN - EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN * EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN / EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN CLAMP EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPONE :OPEN MIX EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPONE :OPEN SMOOTHSTEP EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPONE :OPEN FACEFORWARD EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPONE :OPEN REFRACT EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPONE :OPEN DISTANCE EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DOT EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DISTANCE EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DOT EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DISTANCE EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN DOT EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPONE :OPEN LENGTH EXPTWO :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3)))
  ((EXPONE :OPEN LENGTH EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3)))
  ((EXPONE :OPEN LENGTH EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expone (SYSTEM::BQ-LIST $2 $3)))
  ((EXPTWO :OPEN VEC2 EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN VEC2 EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN SIN EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN COS EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN TAN EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN ASIN EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN ACOS EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN ATAN EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN RADIANS EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN DEGREES EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN EXP EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN LOG EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN EXP2 EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN SQRT EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN INVERSESQRT EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN ABS EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN CEIL EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN FLOOR EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN FRACT EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN SIGN EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3)))  
  ((EXPTWO :OPEN NORMALIZE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN DFDX EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN DFDY EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3)))
  ((EXPTWO :OPEN FWIDTH EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTWO :OPEN ATAN EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN POW EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN MIN EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN MAX EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN MOD EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN STEP EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN REFLECT EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN + EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN - EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN * EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN / EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN MIN EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN MAX EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN MOD EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN + EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN - EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN * EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN / EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN CLAMP EXPTWO EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTWO :OPEN CLAMP EXPTWO EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTWO :OPEN MIX EXPTWO EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTWO :OPEN SMOOTHSTEP EXPTWO EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTWO :OPEN FACEFORWARD EXPTWO EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPTWO :OPEN REFRACT EXPTWO EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPTWO :OPEN MIX EXPTWO EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTWO :OPEN SMOOTHSTEP EXPONE EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTWO :OPEN STEP EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN + EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN - EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN * EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTWO :OPEN / EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :exptwo (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN VEC3 EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN VEC3 EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN VEC3 EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN VEC3 EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN SIN EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN COS EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN TAN EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN ASIN EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN ACOS EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN ATAN EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN RADIANS EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN DEGREES EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN EXP EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN LOG EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN EXP2 EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN SQRT EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN INVERSESQRT EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN ABS EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN CEIL EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN FLOOR EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN FRACT EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN SIGN EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN NORMALIZE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN DFDX EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN DFDY EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3)))
  ((EXPTHREE :OPEN FWIDTH EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPTHREE :OPEN ATAN EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN POW EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN MIN EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN MAX EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN MOD EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN STEP EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN REFLECT EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN CROSS EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN + EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN - EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN * EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN / EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN MIN EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN MAX EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN MOD EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN + EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN - EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN * EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN / EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN CLAMP EXPTHREE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN CLAMP EXPTHREE EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN MIX EXPTHREE EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN SMOOTHSTEP EXPTHREE EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN FACEFORWARD EXPTHREE EXPTHREE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN REFRACT EXPTHREE EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPTHREE :OPEN MIX EXPTHREE EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN SMOOTHSTEP EXPONE EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPTHREE :OPEN STEP EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN + EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN - EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN * EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPTHREE :OPEN / EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expthree (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN VEC4 EXPONE EXPONE EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5 $6))) 
  ((EXPFOUR :OPEN VEC4 EXPONE EXPTHREE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN VEC4 EXPTHREE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN VEC4 EXPTWO EXPTWO :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN VEC4 EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN VEC4 EXPONE EXPTWO EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPFOUR :OPEN VEC4 EXPTWO EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPFOUR :OPEN VEC4 EXPONE EXPONE EXPTWO :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPFOUR :OPEN SIN EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN COS EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN TAN EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN ASIN EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN ACOS EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN ATAN EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN RADIANS EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN DEGREES EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN EXP EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN LOG EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN EXP2 EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN SQRT EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN INVERSESQRT EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN ABS EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN CEIL EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN FLOOR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN FRACT EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN SIGN EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN NORMALIZE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN DFDX EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN DFDY EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN FWIDTH EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3))) 
  ((EXPFOUR :OPEN ATAN EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN POW EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN MIN EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN MAX EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN MOD EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN STEP EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN REFLECT EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN + EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN - EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN * EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN / EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN MIN EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN MAX EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN MOD EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN + EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN - EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN * EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN / EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN CLAMP EXPFOUR EXPONE EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN CLAMP EXPFOUR EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN MIX EXPFOUR EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN SMOOTHSTEP EXPFOUR EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN FACEFORWARD EXPFOUR EXPFOUR EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN REFRACT EXPFOUR EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5)))
  ((EXPFOUR :OPEN MIX EXPFOUR EXPFOUR EXPONE :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN EXPONE EXPONE EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN SMOOTHSTEP EXPONE EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4 $5))) 
  ((EXPFOUR :OPEN STEP EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN + EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN - EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN * EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ((EXPFOUR :OPEN / EXPONE EXPFOUR :CLOSE) (SYSTEM::BQ-LIST :expfour (SYSTEM::BQ-LIST $2 $3 $4))) 
  ;; Functions & operators
  ((+ :+) (SYSTEM::BQ-LIST :+ $1)) 
  ((- :-) (SYSTEM::BQ-LIST :- $1)) 
  ((* :*) (SYSTEM::BQ-LIST :* $1)) 
  ((/ :/) (SYSTEM::BQ-LIST :/ $1)) 
  ((VEC4 :VEC4) (SYSTEM::BQ-LIST :VEC4 $1)) 
  ((CROSS :CROSS) (SYSTEM::BQ-LIST :CROSS $1)) 
  ((VEC3 :VEC3) (SYSTEM::BQ-LIST :VEC3 $1)) 
  ((VEC2 :VEC2) (SYSTEM::BQ-LIST :VEC2 $1)) 
  ((FACEFORWARD :FACEFORWARD) (SYSTEM::BQ-LIST :FACEFORWARD $1)) 
  ((SMOOTHSTEP :SMOOTHSTEP) (SYSTEM::BQ-LIST :SMOOTHSTEP $1)) 
  ((MIX :MIX) (SYSTEM::BQ-LIST :MIX $1)) 
  ((CLAMP :CLAMP) (SYSTEM::BQ-LIST :CLAMP $1)) 
  ((REFLECT :REFLECT) (SYSTEM::BQ-LIST :REFLECT $1)) 
  ((REFRACT :REFRACT) (SYSTEM::BQ-LIST :REFRACT $1)) 
  ((DOT :DOT) (SYSTEM::BQ-LIST :DOT $1)) 
  ((DISTANCE :DISTANCE) (SYSTEM::BQ-LIST :DISTANCE $1)) 
  ((STEP :STEP) (SYSTEM::BQ-LIST :STEP $1)) 
  ((MOD :MOD) (SYSTEM::BQ-LIST :MOD $1)) 
  ((MAX :MAX) (SYSTEM::BQ-LIST :MAX $1)) 
  ((MIN :MIN) (SYSTEM::BQ-LIST :MIN $1)) 
  ((POW :POW) (SYSTEM::BQ-LIST :POW $1)) 
  ((FWIDTH :FWIDTH) (SYSTEM::BQ-LIST :FWIDTH $1)) 
  ((DFDX :DFDX) (SYSTEM::BQ-LIST :DFDX $1)) 
  ((DFDY :DFDY) (SYSTEM::BQ-LIST :DFDY $1))
  ((NORMALIZE :NORMALIZE) (SYSTEM::BQ-LIST :NORMALIZE $1)) 
  ((LENGTH :LENGTH) (SYSTEM::BQ-LIST :LENGTH $1)) 
  ((SIGN :SIGN) (SYSTEM::BQ-LIST :SIGN $1)) 
  ((FRACT :FRACT) (SYSTEM::BQ-LIST :FRACT $1)) 
  ((FLOOR :FLOOR) (SYSTEM::BQ-LIST :FLOOR $1)) 
  ((CEIL :CEIL) (SYSTEM::BQ-LIST :CEIL $1)) 
  ((ABS :ABS) (SYSTEM::BQ-LIST :ABS $1)) 
  ((INVERSESQRT :INVERSESQRT) (SYSTEM::BQ-LIST :INVERSESQRT $1)) 
  ((SQRT :SQRT) (SYSTEM::BQ-LIST :SQRT $1)) 
  ((EXP2 :EXP2) (SYSTEM::BQ-LIST :EXP2 $1)) 
  ((LOG :LOG) (SYSTEM::BQ-LIST :LOG $1)) 
  ((EXP :EXP) (SYSTEM::BQ-LIST :EXP $1)) 
  ((DEGREES :DEGREES) (SYSTEM::BQ-LIST :DEGREES $1)) 
  ((RADIANS :RADIANS) (SYSTEM::BQ-LIST :RADIANS $1)) 
  ((ATAN :ATAN) (SYSTEM::BQ-LIST :ATAN $1)) 
  ((ACOS :ACOS) (SYSTEM::BQ-LIST :ACOS $1)) 
  ((ASIN :ASIN) (SYSTEM::BQ-LIST :ASIN $1)) 
  ((TAN :TAN) (SYSTEM::BQ-LIST :TAN $1)) 
  ((COS :COS) (SYSTEM::BQ-LIST :COS $1)) 
  ((SIN :SIN) (SYSTEM::BQ-LIST :SIN $1))
  ;; Vars
  ((EXPONE VAR1) (SYSTEM::BQ-LIST :expone $1)) 
  ((EXPTWO VAR2) (SYSTEM::BQ-LIST :exptwo $1)) 
  ((EXPTHREE VAR3) (SYSTEM::BQ-LIST :expthree $1)) 
  ((EXPFOUR VAR4) (SYSTEM::BQ-LIST :expfour $1)) 
  ((EXPONE :FLOAT) (SYSTEM::BQ-LIST :expone $1)) 
  ((VAR1 :VAR1) (SYSTEM::BQ-LIST :VAR1 $1)) 
  ((VAR2 :VAR2) (SYSTEM::BQ-LIST :VAR2 $1)) 
  ((VAR3 :VAR3) (SYSTEM::BQ-LIST :VAR3 $1)) 
  ((VAR4 :VAR4) (SYSTEM::BQ-LIST :VAR4 $1))))))


;(setf ll (create-language-from 'GLSL-GRAMMAR-TEST '((x :var1) (y :var3))))
;(parse (grammar ll) '(VEC3 0.0))
;(parse (grammar ll) '(VEC3 (SIN 0.0) (SIN 0.0) (SIN 1.0)))
;(parse (grammar ll) '(+ (VEC3 (SIN 0.0) (SIN 0.0) (SIN 1.0)) (VEC3 (SIN 0.0) (SIN 0.0) (SIN 1.0))))
;(parse (grammar ll) '(+ (VEC3 (SIN 0.0)) (VEC3 (SIN 0.0) (SIN 0.0) (SIN 1.0))))
;(parse (grammar ll) '(+ (VEC3 (SIN 0.0)) 1.0))
;(parse (grammar ll) 'x)
;(parse (grammar ll) '(+ x y))

;(setf ll (create-language-from 'test '((AUXA :VAR1) (AUXB :VAR2) (AUXC :VAR3) (AUXD :VAR4))))
;(setf oo (system-get 'mutate-cfg))
;(dotimes (i 20) (print (mutate-cfg-from 'AUXA ll oo 'expone)))
;(dotimes (i 20) (print (mutate-cfg-from '(vec2 AUXA) ll oo 'exptwo)))
;(dotimes (i 20) (print (mutate-cfg-from '(vec3 AUXA) ll oo 'expthree)))
;(dotimes (i 20) (print (mutate-cfg-from '(vec4 AUXA) ll oo 'expfour)))

;(setf ll (create-language-from 'test '((AUXA :VAR1) (AUXB :VAR2) (AUXC :VAR3))))
;(setf oo (system-get 'mutate-cfg))
;(dotimes (i 100) (print (mutate-cfg-from 'AUXA ll oo 'expone)))
;(dotimes (i 100) (print (mutate-cfg-from '(vec2 AUXA) ll oo 'exptwo)))
;(dotimes (i 100) (print (mutate-cfg-from '(vec3 AUXA) ll oo 'expthree)))
;(dotimes (i 100) (print (mutate-cfg-from '(vec4 AUXA) ll oo 'expfour)))



;; #HACK: Just hardcoded optimization
(defclass glsl-grammar (context-free-grammar)
  ())

(defun glsl-minimum-production-sizes (vars) 
  (LET ((table (MAKE-HASH-TABLE))) 
    (SET-HASH table (QUOTE SIGN) 1 (QUOTE MAX) 1 (QUOTE DFDX) *infinite-productions-size-value* (QUOTE DFDY) *infinite-productions-size-value* (QUOTE MIX) 1 (QUOTE DISTANCE) 1 (QUOTE VEC2) 1 (QUOTE MOD) 1 (QUOTE EXP) 1 (QUOTE CLAMP) 1 (QUOTE START) 1 (QUOTE FRACT) 1 (QUOTE /) 1 (QUOTE TAN) 1 (QUOTE ABS) 1 (QUOTE SIN) 1 (QUOTE ASIN) 1 (QUOTE EXPFOUR) 1 (QUOTE SQRT) 1 (QUOTE CEIL) 1 (QUOTE +) 1 (QUOTE LENGTH) 1 (QUOTE ACOS) 1 (QUOTE STEP) 1 (QUOTE EXPTHREE) 1 (QUOTE INVERSESQRT) 1 (QUOTE REFLECT) 1 (QUOTE REFRACT) 1 (QUOTE MIN) 1 (QUOTE FLOOR) 1 (QUOTE -) 1 (QUOTE EXPTWO) 1 (QUOTE NORMALIZE) 1 (QUOTE DOT) 1 (QUOTE EXPS) 1 (QUOTE COS) 1 (QUOTE EXPONE) 1 (QUOTE POW) 1 (QUOTE VEC4) 1 (QUOTE DEGREES) 1 (QUOTE FACEFORWARD) 1 (QUOTE CROSS) 1 (QUOTE RADIANS) 1 (QUOTE *) 1 (QUOTE LOG) 1 (QUOTE ATAN) 1 (QUOTE SMOOTHSTEP) 1 (QUOTE FWIDTH) 1 (QUOTE VEC3) 1 (QUOTE VAR1) *infinite-productions-size-value* (QUOTE VAR2) *infinite-productions-size-value* (QUOTE VAR3) *infinite-productions-size-value* (QUOTE VAR4) *infinite-productions-size-value*)
    (if (find :var1 vars :test (lambda (o value) (eql (second value) :var1)))
        (SET-HASH table 'VAR1 1))
    (if (find :var2 vars :test (lambda (o value) (eql (second value) :var2)))
        (SET-HASH table 'VAR2 1)
      (SET-HASH table 'EXPTWO 3))
    (if (find :var3 vars :test (lambda (o value) (eql (second value) :var3)))
        (SET-HASH table 'VAR3 1)
      (SET-HASH table 'EXPTHREE 4))
    (if (find :var4 vars :test (lambda (o value) (eql (second value) :var4)))
        (SET-HASH table 'VAR4 1)
      (SET-HASH table 'EXPFOUR 5))
    ;; #FIX: to avoid generation of fwidth, not used for parsing
    (SET-HASH table 'FWIDTH *infinite-productions-size-value*)
    table))

;; #TODO: fix, now we have to set it externally
(defmethod calculate-minimum-production-size ((g glsl-grammar))
  (setf (minimum-production-sizes g) nil))

;; #TODO: Move to context-free-language class
(defclass glsl-language (cfg-tree-language)
  ())

(defmethod can-create-token ((language glsl-language) token)
  "Answer whether <language> can create <token> in a new random expression." 
  (or (structural-symbol token)
      (find-if (lambda (value) (equal token (car value)))
               (tokens (grammar language)))))

(defmethod create-random-token (language (token (eql :FLOAT)))
  "Answer a random value for <token>."
  (create-constant (constants-strategy language)))

(defun mutate-cfg-from (program language operator from)
  (let ((weight-function (production-selection-weight-function operator)))
    (directed-crossover-cfg 
     (create-random-from-production language (list from) (max-size language) weight-function)
     program
     language 
     operator)))
