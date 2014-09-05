
(defparameter *lisp-rgb-vector-tokens*
  '(;; 1 argument operators
    (vecabs :1-ary-operator)
    (vecsqr :1-ary-operator)
    (vecsin :1-ary-operator)
    (veccos :1-ary-operator)
    (vectan :1-ary-operator)
    ;; 2 argument operators
    (vecadd :2-ary-operator)
    (vecsubstract :2-ary-operator)
    (vecmultiply :2-ary-operator)
    (vecdiv :2-ary-operator)
    ;; 3 argument operators
    (veccolormap :3-ary-operator)
    (createvector :create-vector)))

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
               ((scalar constant)
                `(:scalar ,$1))
               ((scalar var)
                `(:scalar ,$1))
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

(system-add
 (make-instance 'context-free-grammar
                :name 'lisp-rgb-vector-images-grammar
                :lexer 'rgb-vector-expression-lexer
                :parser-initializer 'initialize-rgb-vector-expression-parser
                :productions (rgb-vector-expression-grammar-productions)
                :crossover-tokens '(:1-ary-operator :2-ary-operator :3-ary-operator :exp :vector)))

(system-add
 (make-instance 'cfg-tree-language 
                :name 'rgb-color-images-vector
                :description "RGB images vector"
                :grammar (system-get-copy 'lisp-rgb-vector-images-grammar)
                :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                :max-size 40
                :tokens *lisp-rgb-vector-tokens*
                :functions '((vecadd 2) (vecsubstract 2) (vecmultiply 2) (vecdiv 2) (vecabs 1) (vecsqr 1) (vecsin 1) (veccos 1) (vectan 1) (veccolormap 2) (createvector 1))
                :terminals '(x y :constant)
                :variables '(x y)
                :valid-new-expresion-function 'create-new-random-valid
                :simplification-function 'simplify-strategy
                :operators (default-genetic-operators-probability-lisp-expression)))

#|
 (let* ((language (copy-cyclic (system-get 'rgb-color-images-vector)))
        (max-size 30))
    (setf (max-size language) max-size)
    (let ((result (create-random-from-production language '(start) max-size nil)))
      (format nil "~A | ~A" result (infix-coverted-string result))))
|#