;; #TODO: - Move to grammar / language
;;        - Avoid difference between 0.0 and 0
;;        - Add functions and not just symbols with ?
;;        	- Add constants simplification with above consideration
;;
;; #TODO: Try to make something lie this
;;  (((only-contains '+ :constant :variable)) (group-add))
;;  (((only-contains '* :constant :variable)) (group-substract))
;;

(defvar *lisp-math-expression-simplification-patterns*
  '(((+ ?exp 0)        ?1)
    ((+ ?exp 0.0)      ?1)
    ((+ 0 ?exp)        ?2)
    ((+ 0.0 ?exp)      ?2)
    ((- ?exp 0)        ?1)
    ((- ?exp 0.0)      ?1)
    ((- ?exp ?exp)     0)
    ((* ?exp 0)        0)
    ((* ?exp 0.0)      0)
    ((* 0 ?exp)        0)
    ((* 0.0 ?exp)      0)
    ((* ?exp 1)        ?1)
    ((* ?exp 1.0)      ?1)
    ((* 1 ?exp)        ?2)
    ((* 1.0 ?exp)      ?2)
    ((/- ?exp 1)       ?1)
    ((/- ?exp 1.0)     ?1)
    ((/- 0 ?exp)       0)
    ((/- 0.0 ?exp)     0)
    ((/- ?exp 0)       ?1)
    ((/- ?exp 0.0)     ?1)
    ((/- ?exp ?exp)    1)))

;; #TODO: Do it again but destructive operations, this version is slow
(defmethod simplify-strategy (expression strategy language)
  "Answer <expression> simplified."
  (let ((new-expression (simplify-constants expression strategy language)))
    (simplify-patterns new-expression strategy language)))

(defun simplify-patterns (exp strategy language)
  "Answer a simplification of <exp> on <language> using <strategy>.
   #NOTE: Iterates over a set of patterns and applies a reduction when possible. 
   Finish when no reduction is possible."
  (let* ((simple (if (consp exp)
                              (cons (simplify-patterns (car exp) strategy language)
                                    (simplify-patterns (cdr exp) strategy language))
                            exp))
         (result simple))
    ;; For each patter, check if we can simplify current subtree
    (if (consp simple)
        (dolist (i (simplification-patterns language))
          (let ((pattern (car i))
                (result (cadr i)))
            (if (match simple pattern)
                ;; Replace by simplified pattern
                (setf result (apply-result simple result))))))
    ;; Answer the last reduction
    result))

(defun simplify-constants (exp strategy language)
  "Answer a simplification of <exp> on <language> using <strategy>."
  (if (atom exp)
      exp
    (if (and (subexp-constant-p exp language)
             (function-symbol-p (car exp) language))
        (eval exp)
      (cons (if (subexp-constant-p (car exp) language) 
                (if (consp (car exp)) (eval (car exp)) (car exp))
              (simplify-constants (car exp) strategy language))
            (simplify-constants (cdr exp) strategy language)))))

(defun apply-result (exp result)
  (if (symbolp result)
      (if (eql (aref (symbol-name result) 0) #\?)
          (nth (read-from-string (subseq (symbol-name result) 1)) exp)
        result)
    result))


#|
;; #TODO: Improve with these examples
;;           - (+ (/ x x) (/ x x)) (- (+ (/ x x) (/ x x)) (+ (/ x x) (/ x x)))
;;                >> This should be reduced
;;           - (/ x (* x x))
;;                >> This too (order commutative operator arguments and compare)
;;           - (- (+ 1 2) (+ 2 1))

(defmethod simplify (exp (a search-algorithm))
  "Make a function simpler by some ad-hoc method sufficient at the moment."
  (let ((reduced t))
    (do ((...))
        (multiple-value-bind (expresion-simplificada flag) 
            (simplify-subtree exp)
          (setf reduced flag)
          (if (reduced (setf exp expresion-simplificada))))))

;; Some simplification rules
'(((-  exp exp)   0)
  ((/- exp exp)   1)
  ((-  exp 0)     exp)
  ((+  exp 0)     exp)
  ((*  exp 0)     0)
  ((/- 0   exp)   0)
  ((+  0   exp)   exp)
  ((*  0   exp)   0))

;; Things like '(+ (+ (+ x x) x) x) , (* (* x x) (* x x)) , (/ x (+ x x)) , (/ x (* x x))
(+   (expresion-with-just exp '+ 'var)  (* (count exp var) var))
 
|#
