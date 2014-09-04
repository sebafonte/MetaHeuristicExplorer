
(defparameter *lisp-rgb-vector-tokens*
  '(;; 1 argument operators
    (vec-abs :1-ary-operator)
    (vec-sin :1-ary-operator)
    (vec-cos :1-ary-operator)
    (vec-tan :1-ary-operator)
    ;; 2 argument operators
    (vec-+ :2-ary-operator)
    (vec-- :2-ary-operator)
    (vec-* :2-ary-operator)
    (vec-/- :2-ary-operator)
    ;; 3 argument operators
    (color-map :3-ary-operator)
    (cvec3 :3-ary-operator)))


(defun rgb-vector-expression-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (rgb-vector-expression-get-token grammar symbol)
      nil)))

(defun rgb-vector-expression-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (when (equal token-type :unknown)
        (if (numberp word) 
            (setf token-type :constant))
        (setf token-type :constant))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-rgb-vector-expression-parser (name)
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
               ((constant :constant)
                `(:constant ,$1))
               ((var :var)
                `(:var ,$1)))))

(defun rgb-vector-expression-grammar-productions ()
  '((start exp)
    (exp :open 1-ary-operator exp :close)
    (exp :open 2-ary-operator exp exp :close)
    (exp :open 3-ary-operator exp exp exp :close)
    (exp vector)
    (vector :open :create-vector scalar scalar scalar :close)
    (scalar constant)
    (scalar var)
    (constant :constant)
    (var :var)))

(make-instance 'context-free-grammar
               :name 'lisp-rgb-vector-images-grammar
               :lexer 'rgb-vector-expression-lexer
               :parser-initializer 'initialize-rgb-vector-expression-parser
               :productions (rgb-vector-expression-grammar-productions)
               :crossover-tokens '(:1-ary-operator :2-ary-operator :3-ary-operator :exp :vector))

(make-instance 'cfg-tree-language 
               :name 'rgb-color-images-vector
               :description "RGB images vector"
               :grammar (system-get-copy 'lisp-rgb-vector-images-grammar)
               :constants-strategy (system-get-copy 'default-ephemeral-constant-0-1d)
               :max-size 40
               :tokens *lisp-rgb-images-expression-tokens*
               :functions '((vecadd 2) (vecsubstract 2) (vecmultiply 2) (vecdiv 2) (vecabs 1) (vecsqr 1) (vecsin 1) (veccos 1) (vectan 1) (veccolormap 2))
               :terminals '(x y :constant)
               :variables '(x y)
               :valid-new-expresion-function 'create-new-random-valid
               :simplification-function 'simplify-strategy
               :operators (default-genetic-operators-probability-lisp-expression))