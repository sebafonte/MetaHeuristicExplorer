
(defun initialize-default-grammars ()
  (system-add
   ;; Simple math lisp subexpressiones
   (make-instance 'context-free-grammar
                  :name 'lisp-math-function-grammar-x
                  :lexer 'lisp-math-expression-lexer
                  :parser-initializer 'initialize-lisp-math-expression-parser
                  :productions (lisp-math-grammar-productions)
                  :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   (make-instance 'context-free-grammar
                  :name 'lisp-math-function-grammar-x-y 
                  :lexer 'lisp-math-expression-lexer
                  :parser-initializer 'initialize-lisp-math-expression-parser
                  :productions (lisp-math-grammar-productions)
                  :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   (make-instance 'context-free-grammar
                  :name 'lisp-math-function-grammar-x-y-z
                  :lexer 'lisp-math-expression-lexer
                  :parser-initializer 'initialize-lisp-math-expression-parser
                  :productions (lisp-math-grammar-productions)
                  :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   ;; Simple math lisp subexpressiones with subroutines
   (make-instance 'context-free-grammar-with-subroutines
                  :name 'lisp-math-expression-with-subroutines-grammar-x
                  :lexer 'lisp-math-expression-with-subroutines-lexer
                  :parser-initializer 'initialize-lisp-math-expression-with-subroutines-parser
                  :productions (lisp-math-expression-with-subroutines-grammar-productions)
                  :crossover-nodes '(:0-ary-operator :1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   (make-instance 'context-free-grammar-with-subroutines
                  :name 'lisp-math-expression-with-subroutines-grammar-x-y 
                  :lexer 'lisp-math-expression-with-subroutines-lexer
                  :parser-initializer 'initialize-lisp-math-expression-with-subroutines-parser
                  :productions (lisp-math-expression-with-subroutines-grammar-productions)
                  :crossover-nodes '(:0-ary-operator :1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   (make-instance 'context-free-grammar-with-subroutines
                  :name 'lisp-math-expression-with-subroutines-grammar-x-y-z
                  :lexer 'lisp-math-expression-with-subroutines-lexer
                  :parser-initializer 'initialize-lisp-math-expression-with-subroutines-parser
                  :productions (lisp-math-expression-with-subroutines-grammar-productions)
                  :crossover-nodes '(:0-ary-operator :1-ary-operator :2-ary-operator :3-ary-operator :expresion))

   ;; RGB images grammar
   (make-instance 'context-free-grammar
                  :name 'lisp-rgb-images-grammar
                  :lexer 'rgb-expression-lexer
                  :parser-initializer 'initialize-rgb-expression-parser
                  :productions (rgb-expression-grammar-productions)
                  :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   ;; RGB images grammar with subroutines
   (make-instance 'context-free-grammar-with-subroutines
                  :name 'lisp-rgb-images-with-subroutines-grammar
                  :lexer 'rgb-expression-with-subroutines-lexer
                  :parser-initializer 'initialize-rgb-expression-with-subroutines-parser
                  :productions (rgb-expression-grammar-with-subroutines-productions)
                  :crossover-nodes '(:0-ary-operator :1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   ;; Texture deformation grammars
   (make-instance 'context-free-grammar
                  :name 'lisp-rgb-images-grammar-enclosure
                  :lexer 'texture-deformation-enclosure-expression-lexer
                  :parser-initializer 'initialize-texture-deformation-enclosure-expression-parser
                  :productions (texture-deformation-enclosure-expression-grammar-productions)
                  :crossover-nodes '(:values :1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   (make-instance 'context-free-grammar
                  :name 'lisp-rgb-images-grammar-separate
                  :lexer 'texture-deformation-separate-expression-lexer
                  :parser-initializer 'initialize-texture-deformation-separate-expression-parser
                  :productions (texture-deformation-separate-expression-grammar-productions)
                  :crossover-nodes '(:1-ary-operator :2-ary-operator :3-ary-operator :expresion))
   ;; Polynomial expressiones
   (make-instance 'context-free-grammar
                  :name 'polinomyal-grammar-x 
                  :lexer 'polynomial-expression-lexer
                  :parser-initializer 'initialize-polynomial-expression-parser
                  :productions (polynomial-grammar-productions)
                  :crossover-nodes '(:var :constant :termino :productoria-variables :expresion))
   (make-instance 'context-free-grammar
                  :name 'polinomyal-grammar-x-y
                  :lexer 'polynomial-expression-lexer
                  :parser-initializer 'initialize-polynomial-expression-parser
                  :productions (polynomial-grammar-productions)
                  :crossover-nodes '(:var :constant :termino :productoria-variables :expresion))
   (make-instance 'context-free-grammar
                  :name 'polinomyal-grammar-x-y-z
                  :lexer 'polynomial-expression-lexer
                  :parser-initializer 'initialize-polynomial-expression-parser
                  :productions (polynomial-grammar-productions)
                  :crossover-nodes '(:var :constant :termino :productoria-variables :expresion))
   ;; Infix math expression
   (make-instance 'context-free-grammar
                  :name 'infix-math-expression-grammar
                  :lexer 'lisp-math-expression-lexer
                  :parser-initializer 'initialize-infix-math-expression-parser)
   ;; OpenCL parsing grammar
   (make-instance 'context-free-grammar
                  :name 'cl-parsing-grammar
                  :lexer 'lisp-math-expression-lexer
                  :tokens *lisp-math-expression-cl-tokens*
                  :parser-initializer 'initialize-cl-expression-parser)
   ;; Evolutive algorithm
   (make-instance 'context-free-grammar
                  :name 'search-algorithm-grammar
                  :lexer 'search-algorithm-grammar-lexer
                  :parser-initializer 'initialize-search-algorithm-grammar-parser
                  :productions (search-algorithm-grammar-productions)
                  :tokens *search-algorithm-grammar-tokens*
                  :crossover-nodes '(:constant
                                      :elite-manager-description
                                      :selection-method-description
                                      :initializer-operation-description
                                      :evolver-description))
	;; Search task 
   (make-instance 'search-task-grammar
                  :name 'search-task-sample-grammar
                  :lexer 'search-task-grammar-lexer
                  :parser-initializer 'initialize-search-task-grammar-parser
                  :productions (search-task-grammar-productions)
                  :tokens *search-task-grammar-tokens*
                  :crossover-nodes '(:constant
                                      :search-object-description
                                      :task-description-list
                                      :builder-description
                                      :elite-manager-description
                                      :selection-method-description))))
  

