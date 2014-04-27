(defclass test-node-selection-functions-cfg (test-base-model) 
  ())


(defmethod default-expression ((o test-node-selection-functions-cfg))
  "Answer default expression for <o> test cases."
  '(+ (* 1 y) (- (* 9 x) 3)))

(defmethod default-grammar ((o test-node-selection-functions-cfg))
  (grammar (system-get 'lisp-math-function-xy)))

(defmethod local-test-node ((o test-node-selection-functions-cfg) expresion function results)
  "Verifies whether result of selecting subtrees from expresion using function are within <results>."
  (let ((algorithm (default-algorithm o))
        (parse-tree (parse (default-grammar o) expresion)))
    (dotimes (i 50)
        (let* ((index (1+ (select-random-index-cfg parse-tree function algorithm)))
               (selection (select-subtree-cfg parse-tree index))
               (value (compress-flatten-parenthesis-token-value selection)))
          (check 
            (includes results value))))))

(defmethod test-node-selection-function-terminals ((o test-node-selection-functions-cfg))
  "Verify selection on terminal nodes performing selections of <o> default expression."
  (test-node 
   o 
   (default-expression o)
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (or (eql (car parse-tree) :constant)
             (eql (car parse-tree) :var))
         1 0))
   '(1 y 9 y 3 x)))

(defmethod test-constant-subtree-selection-function ((o test-node-selection-functions-cfg))
  "Verify selection on constant nodes performing selections of <o> default expression."
  (test-node 
   o
   (default-expression o)
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (eql (car parse-tree) :constant) 1 0))
   '(1 9 3)))

(defmethod test-variable-subtree-selection-function ((o test-node-selection-functions-cfg))
  "Verify selection on variable nodes performing selections of <o> default expression."
  (local-test-node 
   o 
   (default-expression o)
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (eql (car parse-tree) :var) 1 0))
   '(x y)))

(defmethod test-functions-subtree-selection-function ((o test-node-selection-functions-cfg))
  "Verify selection on variable nodes performing selections of <o> default expression."
  (local-test-node 
   o 
   '(+ (* 1 y) (- (* 9 x) (sin 3)))
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (eql (car parse-tree) :1-ary-operator) 1 0))
   '(sin))
  (local-test-node 
   o 
   '(+ (* 1 y) (- (* 9 x) (sin 3)))
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (eql (car parse-tree) :2-ary-operator) 1 0))
   '(+ * -)))

(defmethod test-subexpressions-selection-function ((o test-node-selection-functions-cfg))
  "Verify selection on subtree nodes performing selections of <o> default expression."
  (local-test-node 
   o
   (default-expression o)
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (eql (car parse-tree) :expresion) 1 0))
   '((+ (* 1 y) (- (* 9 x) 3))
     (- (* 9 x) 3)
     (* 1 y)
     (* 9 x)
     1
     3
     9
     x
     y)))

(defmethod test-node-selection-function-subtrees ((o test-node-selection-functions-cfg))
  "Verify selection on subtree nodes performing selections of <o> default expression."
  (local-test-node 
   o
   (default-expression o)
   (lambda (parse-tree index)
     (declare (ignore index))
     (if (and (eql (car parse-tree) :expresion)
              (> (tree-size (compress-flatten-parenthesis-token-value parse-tree)) 1))
         1 0))
   '((+ (* 1 y) (- (* 9 x) 3))
     (- (* 9 x) 3)
     (* 1 y)
     (* 9 x))))

(defmethod test-multiplication-subtrees-selection-function ((o test-node-selection-functions-cfg))
  "Verify selection on subtree nodes with #'* performing selections of <o> default expression."
  (local-test-node 
   o 
   '(* (* 1 y) (- (* 9 x) (* 3 y)))
   (lambda (parse-tree index)
     (declare (ignore index))
     (let ((subexp (compress-flatten-parenthesis-token-value parse-tree)))
       (if (and (eql (car parse-tree) :expresion)
                (> (tree-size subexp) 1)
                (equal '* (car subexp)))
           1 0)))
   '((* (* 1 y) (- (* 9 x) (* 3 y)))
     (* 1 y)
     (* 9 x)
     (* 3 y))))