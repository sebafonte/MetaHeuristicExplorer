(defclass test-polynomials-language (test-base-model)
  ((grammar :initarg :grammar :accessor grammar)))


(defmethod initialize-instance :after ((o test-polynomials-language) &rest args)
  (setf (grammar o) (grammar (system-get-copy 'polynomial-xy))))

(defmethod good-formed-polynomials-cases ((o test-polynomials-language))
  '((+ 1 (* 2 (* x x)))
    (+ 1 2)
    (+ 1 x)
    (+ 1 x 2)
    (+ 1 x 2 x)
    (+ 1 (* 3 (* x x)))
    (+ 1 (* 3 (* x x)) (* 0.5 (* x x x x)))
    (+ 1 x (* 3 (* x x)) (* 0.5 (* x x x x)))))

(defmethod badly-formed-polynomials-cases ((o test-polynomials-language))
  '((1)
    (+ 1 (* 3 (* x x)) (* 0.5 (* x x x 2)))
    (+ 1 (* 3 (* x x)) (* 0.5 (* 2 x x x)))
    (+ (* x x))))

(defmethod test-parse-polynomials-ok ((o test-polynomials-language))
  (dolist (expression (good-formed-polynomials-cases o))
    (multiple-value-bind (result error)
        (parse (grammar o) expression)
      (declare (ignore result))
      (check (null error)))))
        
(defmethod test-parse-polynomials-errors ((o test-polynomials-language))
  (dolist (expression (badly-formed-polynomials-cases o))
    (multiple-value-bind (result error)
        (parse (grammar o) expression)
      (declare (ignore result))
      (check error))))

(defmethod test-compress-flatten-parenthesis-token-value ((o test-polynomials-language))
  (check (equal 
          (compress-flatten-parenthesis-token-value 
           '(:POLYNOMIAL 
             ((:ADD +) 
              (:CONSTANT 1) 
              (:TERM ((:MULTIPLY *) 
                         (:CONSTANT 2) 
                         (:PRODUCTORIA-VARIABLES ((:MULTIPLY *) (:VAR X) (:VAR X))))))))
          '(+ 1 (* 2 (* X X))))))

(defmethod test-compress-flatten-parenthesis-token-type ((o test-polynomials-language))
  (check 
   (equal 
    (compress-flatten-parenthesis-token-type
     '(:POLYNOMIAL 
       ((:ADD +) 
        (:CONSTANT 1) 
        (:TERM ((:MULTIPLY *) 
                   (:CONSTANT 2) 
                   (:PRODUCTORIA-VARIABLES ((:MULTIPLY *) (:VAR X) (:VAR X))))))))
    '(:ADD :CONSTANT (:MULTIPLY :CONSTANT (:MULTIPLY :VAR :VAR))))))

(defmethod test-flatten-parenthesis ((o test-polynomials-language))
  (check
    (equal (flatten-parenthesis '(+ 1 x (* x x) (* x x x x)))
           '(:OPEN + 1 X :OPEN * X X :CLOSE :OPEN * X X X X :CLOSE :CLOSE))))

(defmethod test-directed-crossover-cfg-test ((o test-polynomials-language))
  (let ((operator (system-get 'crossover-cfg))
        (language (copy (system-get 'polynomial-xy)))
        (a '(+ 1 (* 2 (* x x)) (* 2.5 (* y x))))
        (b '(+ (* 3 (* y y)) 2 (* 4 (* x x)) (* 0.5 (* x x x y))))
        (c 1)
        (d 2))
    (setf (max-size language) 25)
    (dotimes (i 100)
      (check (>= (max-size language) 
                 (tree-size (directed-crossover-cfg a b language operator))))
      (check (>= (max-size language) 
                 (tree-size (directed-crossover-cfg c d language operator))))
      (check (>= (max-size language) 
                 (tree-size (directed-crossover-cfg a d language operator))))
      (check (>= (max-size language) 
                 (tree-size (directed-crossover-cfg d a language operator)))))))
      
