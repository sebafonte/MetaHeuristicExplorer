
(defparameter *lisp-rgb-images-expression-with-subroutines-tokens*
  '(;; 0 argument operators
    (sample-zero-ary :0-ary-operator)
    ;; 1 argument operators
    (vec-abs :1-ary-operator)
    (vec-sin :1-ary-operator)
    (vec-cos :1-ary-operator)
    (vec-tan :1-ary-operator)
    ;; 2 argument operators
    (vec-+ :2-ary-operator)
    (vec-- :2-ary-operator)
    (vec-* :2-ary-operator)
    (vec-/- :2-ary-operator)
    (color-map-1 :2-ary-operator)
    (vec-inoise-x-y :2-ary-operator)
    (vec-perlin-x-y :2-ary-operator)
    ;; 3 argument operators
    (color-map-3 :3-ary-operator)
    (cvec3 :3-ary-operator)))


(defun rgb-expression-with-subroutines-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (rgb-expression-with-subroutines-get-token grammar symbol)
      nil)))

(defun rgb-expression-with-subroutines-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (when (equal token-type :unknown)
      ;; Check for numeric constant
      (if (numberp word) 
          (setf token-type :constant))
      ;; Check for image vector constant
      (if (equal (class-name (class-of word)) 'image-vector-3d)
          (setf token-type :constant)))
    ;; Check for subroutine call
    (if (is-subroutine-call-node word)
        (setf token-type :0-ary-operator))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-rgb-expression-with-subroutines-parser (name)
  (eval
   `(defparser ,name
               ((start expresion)
                $1)
               ((expresion :open :0-ary-operator :close)
                `(:expresion (,$2)))
               ((expresion :open :1-ary-operator expresion :close)
                `(:expresion (,$2 ,$3)))
               ((expresion :open :2-ary-operator expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4)))
               ((expresion :open :3-ary-operator expresion expresion expresion :close)
                `(:expresion (,$2 ,$3 ,$4 ,$5)))
               ((expresion :constant)
                `(:expresion ,$1))
               ((expresion :var)
                `(:expresion ,$1)))))

(defun rgb-expression-grammar-with-subroutines-productions ()
  '((start expresion)
    (expresion :open 0-ary-operator :close)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion constant)
    (expresion var)
    (constant :constant)
    (var :var)))
