
(defparameter *lisp-math-expression-with-subroutines-tokens*
  '(;; 0 argument operators
    (sample-zero-ary :0-ary-operator)
    ;; 1 argument operators
    (abs :1-ary-operator)
    (sin :1-ary-operator)
    (cos :1-ary-operator)
    (tan :1-ary-operator)
    (sqr :1-ary-operator)
    (exp :1-ary-operator)
    (log :1-ary-operator)
    (real-sqrt :1-ary-operator)
    ;; 2 argument operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/ :2-ary-operator)
    (/- :2-ary-operator)
    (real-expt :2-ary-operator)
    ;; 3 argument operators
    (real-if :3-ary-operator)))


(defun lisp-math-expression-with-subroutines-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (lisp-math-expression-with-subroutines-get-token grammar symbol)
      nil)))

(defun lisp-math-expression-with-subroutines-get-token (grammar word)
  "Answer the token type of <word>."
  ;; Check for defined function
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    ;; Check for numeric constant
    (if (equal token-type :unknown)
        (setf token-type (if (numberp word) :constant)))
    ;; Check for subroutine call
    (if (is-subroutine-call-node word)
        (setf token-type :0-ary-operator))
    (values token-type (list token-type word))))

(defun initialize-lisp-math-expression-with-subroutines-parser (name)
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

(defun lisp-math-expression-with-subroutines-grammar-productions ()
  '((start expresion)
    (expresion :open 0-ary-operator :close)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion constant)
    (expresion var)
    (constant :constant)
    (var :var)))
