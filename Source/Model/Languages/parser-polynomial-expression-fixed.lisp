
(defun parser-term-lexer ()
  (let ((symbol (pop *parser-input*)))
    (if symbol (polynomial-expression-get-token-type symbol)
      nil)))

(defun polynomial-expression-get-token-type (word)
  "Answer the token type of word."
  ;; Check a symbol table
  (let ((token-type (search-on-symbol-table *polynomial-expression-tokens* word)))
    ;; Check for numeric constants
    (if (equal token-type :unknown) 
        (setf token-type 
              (if (numberp word) :constant 
                (if (listp word) :list))))
    (if (equal :list token-type)
        (values token-type word)        
      (values token-type token-type))))

(defparameter *polynomial-expression-tokens*
  '(;; Operators
    (+ :add)
    (- :substract)
    (* :multiply)
    (/ :divide)
    (/- :divide)
    ;; Variables
    (x :var)
    (y :var)
    (z :var)
    ;; Constants
    (e :constant)
    (pi :constant)
    (1 :constant)
    (0 :constant)
    (-1 :constant)))

(defparser parser-polynomial
           ;; Start
           ((bs indicator) $1)
           ((indicator polynomial)
            `(indicator-polynomial ,$1))
           ;; Term
           ((polynomial term)
            `(,$1))
           ((polynomial :add terms)
            `(,$1 ,@$2))
           ((terms term terms)
            `(,$1 ,@$2))
           ((terms term)
            `(,$1))
           ((term :constant)
            $1)
           ((term :list)
            (parse-term $1)))
           
(defparser parser-term 
           ;; Start
           ((bs indicator) $1)
           ((indicator term)
            `(indicator-term ,$1))
           ;; Term
           ((term :multiply :constant productoria-variables)
            `(,$1 ,$2 ,$3))
           ((productoria-variables :list)
            (parse-productoria $1))
           ;; Factors
           ((factor :var) $1)
           ((factor :constant) $1))

(defparser parser-productoria
           ;; Start
           ((bs indicator) $1)
           ((indicator productoria-variables)
            `(indicator-productoria ,$1))
           ((productoria-variables :multiply variables)
            `(,$1 ,@$2))
           ((variables :var)
            `(,$1))
           ((variables :var variables)
            `(,$1 ,@$2)))


;; #TEST EXAMPLES
(defun parse-polynomial (expresion)
  (let ((*parser-input* expresion))
    (parser-polynomial #'parser-term-lexer)))

(defun parse-term (expresion)
  (let ((*parser-input* expresion))
    (parser-term #'parser-term-lexer)))

(defun parse-productoria (expresion)
  (let ((*parser-input* expresion))
    (parser-productoria #'parser-term-lexer)))



(defclass test-polynomial-grammar-multiple-parsers (test-base-model) ())

;; #TODO: 
;;   - Make them fail, it does not work, check it out
;;   - Check if (+ x x x x x) is usefull or force it to (+ x (+ x (+ x (+ x x))))
(defmethod test-parser-polinomyal-test-examples ((o test-polynomial-grammar-multiple-parsers))
 "Verifies whether properties can be deleted correctly to an instance of object-with-properties."
 ;; Supported polynomial expression forms
 (parse-polynomial '(2))
 (parse-polynomial '(+ 2 3))
 (parse-polynomial '(+ 2 (* 1 (* x x))))
 (parse-polynomial '(+ 7 (* 5 (* x x y))))
 (parse-polynomial '(+ (* 2 (* x y x)) (* 3 (* x x))))
 (parse-polynomial '(+ (* 1 (* x x)) 
                        (* 2 (* x x y))
                        (* 3 (* x x y y x x)) 
                        (* 4 (* x x y y x x y))))
 (parse-polynomial '(+ 2
                        (* 1 (* x x)) 
                        2
                        (* 3 (* x x y y x x)) 
                        (* 4 (* x x y y x x y)) 
                        5))
 ;; Invalid expressions
 (parse-polynomial '(+ (+ (* 1 (* x x)) 
                           (* 2 (* x x y)))
                        (+ (* 3 (* x x y y)) 
                           (* 4 (* x x y y x)))))
 ;; Not supported at the moment, but if wanted it can be parsed
 (parse-polynomial '(+ 1 (* x x) (* x x x) (* x x x x) (* x x x x x))))

(defmethod test-parser-polinomial-test-term ((o test-polynomial-grammar-multiple-parsers))
  (parse-term '(* 1 (* x)))
  (parse-term '(* 1 (* x x)))
  (parse-term '(* 1 (* x y x y x y))))

(defmethod test-parser-polynomial-test-productoria ((o test-polynomial-grammar-multiple-parsers))
  (parse-productoria '(* x))
  (parse-productoria '(* x x))
  (parse-productoria '(* x x x))
  (parse-productoria '(* x x y y))
  (parse-productoria '(* y x x x x y x x x x x x)))

