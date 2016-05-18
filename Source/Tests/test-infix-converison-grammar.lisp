(defclass test-infix-conversion-grammar (test-base-model) ())


(defmethod default-infix-arithmetic-grammar ((o test-infix-conversion-grammar))
  (system-get-copy 'infix-math-expression-grammar))

(defmethod test-infix-arithmetic-expression-parse ((o test-infix-conversion-grammar))
  "Verifies whether arithmetic infix grammar parsing is working properly with some examples."
  (let ((grammar (default-infix-arithmetic-grammar o)))
    (labels ((test (a b)
               (check (equals (deparse 
                               (parse grammar a)) b))))
      (test '(1 + 2)             '(+ 1 2))
      (test '(X - 2)             '(- X 2))
      (test '(1 + Y)             '(+ 1 Y))
      (test '(X - Y)             '(- X Y))
      (test '(X + Y + 3)         '(+ (+ X Y) 3))
      (test '(1 + 2 + Y)         '(+ (+ 1 2) Y))
      (test '(1 + 2 + X + Y)     '(+ (+ (+ 1 2) X) Y))
      (test '(1 + 2 + X + Y - 7) '(- (+ (+ (+ 1 2) X) Y) 7))
      (test '((1 + X) - X)       '(- (+ 1 X) X))
      (test '((1 - 2) + (X - Y)) '(+ (- 1 2) (- X Y))))))

(defmethod test-infix-arithmetic-expression-token-tree ((o test-infix-conversion-grammar))
  "Verifies whether arithmetic infix grammar for token tree parsing is working properly with some examples."
  (let ((grammar (default-infix-arithmetic-grammar o)))
    (labels ((test (a b)
               (check (equals (compress-flatten-parenthesis-token-type (parse grammar a)) b))))
      (test '(1 + 2)              '(:2-ary-operator :constant :constant))
      (test '(X - 2)              '(:2-ary-operator :var :constant))
      (test '(1 + Y)              '(:2-ary-operator :constant :var))
      (test '(X - Y)              '(:2-ary-operator :var :var))
      (test '(X + Y + 3)          '(:2-ary-operator (:2-ary-operator :var :var) :constant))
      (test '(1 + 2 + Y)          '(:2-ary-operator (:2-ary-operator :constant :constant) :var))
      (test '(1 + 2 + X + Y)      '(:2-ary-operator 
                                       (:2-ary-operator 
                                        (:2-ary-operator :constant :constant) 
                                        :var)
                                       :var))
      (test '(1 + 2 + X + Y - 7)  '(:2-ary-operator 
                                       (:2-ary-operator 
                                        (:2-ary-operator 
                                         (:2-ary-operator :constant :constant) 
                                         :var)
                                        :var)
                                       :constant))
      (test '((1 + X) - X)        '(:2-ary-operator (:2-ary-operator :constant :var) :var))
      (test '((1 - 2) + (X - Y))  '(:2-ary-operator 
                                       (:2-ary-operator :constant :constant) 
                                       (:2-ary-operator :var :var))))))