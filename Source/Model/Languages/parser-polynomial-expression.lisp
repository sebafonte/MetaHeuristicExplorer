
(defparameter *polynomial-expression-tokens*
  '(;; Operators
    (+ :add)
    (* :multiply)))


(defun functions-list-from-tokens (tokens)
  (mapcar (lambda (value) (list (car value) '?))
          tokens))

(defun polynomial-expression-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol (polynomial-expression-get-token grammar symbol)
      nil)))

(defun polynomial-expression-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (if (equal token-type :unknown) 
        (setf token-type (if (numberp word) :constant)))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-polynomial-expression-parser (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start expresion) $1)
               ;; Definitions for arithmetic operations of lower priority
               ((expresion :open :add terms :close)
                `(:expresion (,$2 ,@$3)))
               ;; Terms
               ((terms term)
                `(,$1))
               ((terms terms term)
                `(,@$1 ,$2))
               ;; Term
               ((term factor)
                $1)
               ((term :open :multiply :constant productoria-variables :close)
                `(:term (,$2 ,$3 ,$4)))
               ;; Productoria de variables
               ((productoria-variables :open :multiply variables :close)
                `(:productoria-variables (,$2 ,@$3)))
               ;; Variables
               ((variables :var)
                `(,$1))
               ((variables :var variables)
                `(,$1 ,@$2))
               ;; Factors
               ((factor :var) $1)
               ((factor :constant) $1))))

(defun polynomial-grammar-productions ()
  '((start expresion)
    (expresion :open add terms :close)
    (terms term)
    (terms terms term)
    (term factor)
    (term :open multiply constant productoria-variables :close)
    (productoria-variables :open multiply variables :close)
    (variables var)
    (variables var variables)
    (factor var)
    (factor constant)
    (var :var)
    (constant :constant)))
	