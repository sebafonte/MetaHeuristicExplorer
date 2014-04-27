
(defparameter *lisp-math-expression-tokens*
  '(;; 1 argument operators
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


(defun lisp-math-expression-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (lisp-math-expression-get-token grammar symbol)
      nil)))

(defun lisp-math-expression-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (if (equal token-type :unknown)
        (setf token-type (if (numberp word) :constant)))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-lisp-math-expression-parser (name)
  (eval
   `(defparser ,name
               ((start expresion)
                $1)
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

(defun lisp-math-grammar-productions ()
  '((start expresion)
    (expresion :open 1-ary-operator expresion :close)
    (expresion :open 2-ary-operator expresion expresion :close)
    (expresion :open 3-ary-operator expresion expresion expresion :close)
    (expresion constant)
    (expresion var)
    (constant :constant)
    (var :var)))

