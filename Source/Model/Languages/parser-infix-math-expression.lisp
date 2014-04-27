
;;; Grammar for adaption to infix -> prefix notation

(defun initialize-infix-math-expression-parser (name)
  (eval
   `(defparser ,name
               ((start expresion) $1)
               ;; Define low priority arithmetic operators
               ((expresion :open expresion :2-ary-operator expresion :close)
                `(,$2 ,$1 ,$3))
               ((expresion factor)
                $1)
               ;; Define high priority arithmetic operators
               ;; Factor
               ((factor :var) $1)
               ((factor :constant) $1))))
