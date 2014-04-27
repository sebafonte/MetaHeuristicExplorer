
#|
;; #NOTE: Example for test
(parse (system-get 'polinomyal-grammar-x-y) '(/ (* x y) 7))

;; #NOTE: Example for test
(parse (system-get 'polinomyal-grammar-x-y) '(* 1 x))
(parse (system-get 'polinomyal-grammar-x-y) '(* 1 (* x x)))

;; #NOTE: Example for test
(parse (system-get 'basic-infix-math-expression) '(1 + (x + (2 + y))))
|#