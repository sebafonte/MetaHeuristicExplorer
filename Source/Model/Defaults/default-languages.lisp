
(defun initialize-default-languages ()
  "Initialize system default languages."
  (system-add
   ;; Languages for simple math expressions
   (make-instance 'cfg-tree-language 
                  :name 'lisp-math-function-x 
                  :grammar (system-get-copy 'lisp-math-function-grammar-x)
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x :constant)
                  :variables '(x)
                  :tokens *lisp-math-expression-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-lisp-expression))
   (make-instance 'cfg-tree-language 
                  :name 'lisp-math-function-xy
                  :grammar (system-get-copy 'lisp-math-function-grammar-x-y)
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :tokens *lisp-math-expression-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-lisp-expression))
   (make-instance 'cfg-tree-language 
                  :name 'lisp-math-function-xyz
                  :grammar (system-get-copy 'lisp-math-function-grammar-x-y-z)
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x y z :constant)
                  :variables '(x y z)
                  :tokens *lisp-math-expression-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-lisp-expression))
   ;; Languages for simple math expressions with ADFs
   (make-instance 'adf-tree-language 
                  :name 'adf-lisp-math-function-x
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x :constant)
                  :variables '(x)
                  :tokens *lisp-math-expression-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-adf-lisp-expression))
   (make-instance 'adf-tree-language 
                  :name 'adf-lisp-math-function-xy
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :tokens *lisp-math-expression-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-adf-lisp-expression))
   ;; Languages for simple math expressions with compression
   (make-instance 'compression-tree-language 
                  :name 'compression-lisp-math-function-x
                  ;; Grammar is supplied to allow cfg operations
                  :grammar (system-get-copy 'lisp-math-expression-with-subroutines-grammar-x)
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x :constant)
                  :variables '(x)
                  :tokens *lisp-math-expression-with-subroutines-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-compression-lisp-expression))
   (make-instance 'compression-tree-language
                  :name 'compression-lisp-math-function-xy
                  ;; Grammar is supplied to allow cfg operations
                  :grammar (system-get-copy 'lisp-math-expression-with-subroutines-grammar-x-y)
                  :simplification-patterns *lisp-math-expression-simplification-patterns*
                  :constants-strategy (system-get-copy 'default-fixed-set-numerical-1)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :tokens *lisp-math-expression-with-subroutines-tokens*
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-compression-lisp-expression))
   ;; Languages for rgb images
   (make-instance 'cfg-tree-language 
                  :name 'rgb-color-images
                  :grammar (system-get-copy 'lisp-rgb-images-grammar)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :max-size 40
                  :tokens *lisp-rgb-images-expression-tokens*
                  :functions '((vec-+ 2) (vec-- 2) (vec-* 2) (vec-/- 2) (vec-sin 1) (vec-cos 1) 
                                     (color-map-1 2) (color-map-3 3) (vec-inoise-x-y 2) (vec-perlin-x-y 2))
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-lisp-expression))
   ;; Languages for rgb images with subroutines
   (make-instance 'compression-tree-language 
                  :name 'rgb-color-images-srt
                  :grammar (system-get-copy 'lisp-rgb-images-with-subroutines-grammar)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :max-size 30
                  :tokens *lisp-rgb-images-expression-with-subroutines-tokens*
                  :functions '((vec-+ 2) (vec-- 2) (vec-* 2) (vec-/- 2) (vec-sin 1) (vec-cos 1) 
                                     (color-map-1 2) (color-map-3 3) (vec-inoise-x-y 2) (vec-perlin-x-y 2))
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-strategy
                  :operators (default-genetic-operators-probability-compression-lisp-expression))
   ;; Languages for texture deformation
   (make-instance 'cfg-tree-language 
                  :name 'rgb-color-images-separate
                  :grammar (system-get-copy 'lisp-rgb-images-grammar-separate)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :max-size 30
                  :tokens *texture-deformation-separate-expression-tokens*
                  :functions '((+ 2) (- 2) (* 2) (/- 2) (sin 1) (cos 1) (tan 1) (values 2))
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-texture-deformation-separate
                  :operators (default-genetic-operators-probability-texture-separate))
   (make-instance 'cfg-tree-language 
                  :name 'rgb-color-images-enclosure
                  :grammar (system-get-copy 'lisp-rgb-images-grammar-enclosure)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :max-size 100
                  :tokens *texture-deformation-enclosure-expression-tokens*
                  :functions '((vec-+ 2) (vec-- 2) (vec-* 2) (vec-/- 2) (vec-sin 1) (vec-cos 1))
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :valid-new-expresion-function 'create-new-random-valid
                  :simplification-function 'simplify-texture-deformation-separate
                  :operators (default-genetic-operators-probability-texture-enclosure))
   ;; Languages for polynomial expressions
   (make-instance 'cfg-tree-language 
                  :name 'polynomial-x
                  :grammar (system-get-copy 'polinomyal-grammar-x)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :terminals '(x :constant)
                  :variables '(x)
                  :functions (functions-list-from-tokens *polynomial-expression-tokens*)
                  :tokens *polynomial-expression-tokens*
                  :valid-new-expresion-function 'create-new-first-parent-program-copy
                  :operators (default-genetic-operators-probability-polynomial-expression))
   (make-instance 'cfg-tree-language 
                  :name 'polynomial-xy
                  :grammar (system-get-copy 'polinomyal-grammar-x-y)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :terminals '(x y :constant)
                  :variables '(x y)
                  :functions (functions-list-from-tokens *polynomial-expression-tokens*)
                  :tokens *polynomial-expression-tokens*
                  :valid-new-expresion-function 'create-new-first-parent-program-copy
                  :operators (default-genetic-operators-probability-polynomial-expression))
   (make-instance 'cfg-tree-language 
                  :name 'polynomial-xyz
                  :grammar (system-get-copy 'polinomyal-grammar-x-y-z)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :terminals '(x y z :constant)
                  :variables '(x y z)
                  :functions (functions-list-from-tokens *polynomial-expression-tokens*)
                  :tokens *polynomial-expression-tokens*
                  :valid-new-expresion-function 'create-new-first-parent-program-copy
                  :operators (default-genetic-operators-probability-polynomial-expression))
   ;; Language for search algorithms (generic)
   (make-instance 'cfg-tree-language 
                  :name 'evolutive-algorithm-language
                  :grammar (system-get-copy 'search-algorithm-grammar)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :terminals '(:constant)
                  :variables '()
                  :functions (functions-list-from-tokens *search-algorithm-grammar-tokens*)
                  :tokens *search-algorithm-grammar-tokens*
                  :max-size 100
                  :valid-new-expresion-function 'create-new-first-parent-program-copy
                  :operators (default-genetic-operators-probability-polynomial-expression))
   ;; language for search algorithms (specialized later)
   (make-instance 'cfg-tree-language 
                  :name 'evolutive-algorithm-language-x-y
                  :grammar (system-get-copy 'search-algorithm-grammar)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :terminals '(:constant)
                  :variables '()
                  :functions (functions-list-from-tokens *search-algorithm-grammar-tokens*)
                  :tokens *search-algorithm-grammar-tokens*
                  :max-size 100
                  :valid-new-expresion-function 'create-new-first-parent-program-copy
                  :operators (default-genetic-operators-probability-polynomial-expression))
   (make-instance 'cfg-tree-language 
                  :name 'evolutive-algorithm-language-vrp
                  :grammar (system-get-copy 'search-algorithm-grammar)
                  :constants-strategy (system-get-copy 'default-ephemeral-0-1d)
                  :terminals '(:constant)
                  :variables '()
                  :functions (functions-list-from-tokens *search-algorithm-grammar-tokens*)
                  :tokens *search-algorithm-grammar-tokens*
                  :max-size 100
                  :valid-new-expresion-function 'create-new-first-parent-program-copy
                  :operators (default-genetic-operators-probability-polynomial-expression))
   ;; VRP languages
   (make-instance 'vrp-list-language 
                  :name 'vrp-default-language
                  :operators (default-genetic-operators-probability-sample-vrp)
                  :valid-new-expresion-function 'create-new-first-parent-copy)
   ;; Linear ordering languages
   (make-instance 'language 
                  :name 'lop-default-language
                  :operators (default-genetic-operators-probability-linear-ordering)
                  :valid-new-expresion-function 'create-new-first-parent-copy)
   ;; Linear ordering with lists languages
   (make-instance 'language 
                  :name 'lop-lists-default-language
                  :operators (default-genetic-operators-probability-linear-ordering)
                  :valid-new-expresion-function 'create-new-first-parent-copy)
   ;; Search task languages
   (make-instance 'cfg-tree-language 
                  :name 'search-task-default-language
                  :grammar (system-get-copy 'search-task-sample-grammar)
                  :functions (functions-list-from-tokens *search-task-grammar-tokens*)
                  :operators (default-genetic-operators-probability-search-task)
                  :valid-new-expresion-function 'create-new-first-parent
                  :tokens *search-task-grammar-tokens*)
   ;; Lisp -> CL translation language
   (make-instance 'cfg-tree-language 
                  :name 'lisp-cl-translation-language
                  :grammar (system-get-copy 'cl-parsing-grammar)
                  :functions (entity-function-default-functions-info)
                  :terminals '(x y z :constant)
                  :variables '(x y z)
                  :tokens *lisp-math-expression-cl-tokens*))
  ;; Specialize some languages
  (specialize-language-from 
   (system-get 'evolutive-algorithm-language-x-y)
   (system-get 'lisp-math-function-xy))
  (specialize-language-from 
   (system-get 'evolutive-algorithm-language-vrp)
   (system-get 'vrp-default-language)))

(defun entity-function-default-functions-info ()
  '((+ 2) (- 2) (* 2) (/- 2) (sin 1) (cos 1) (sqr 1) (abs 1)))

(defmethod noise-functions ((o entity-function-x-y))
  '((perlin1 1) 
    (perlin2 2)
    (perlin-noise-x-y 2) 
    (interpolated-noise 2)))
