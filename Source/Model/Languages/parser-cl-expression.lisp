
;;; Grammar for adaption to prefix -> infix notation for a CL (C99) subset

(defparameter *lisp-math-expression-cl-tokens*
  '(;; Functions
    (abs :function-1)
    (sin :function-1)
    (cos :function-1)
    (tan :function-1)
    (sqr :function-1)
    (exp :function-1)
    (log :function-1)
    (real-sqrt :function-1)
    (real-expt :function-2)
    (/- :function-2)
    ;; Operators
    (+ :2-ary-operator)
    (- :2-ary-operator)
    (* :2-ary-operator)
    (/ :2-ary-operator)
    ;; Other
    (real-if :real-if)))

(defun initialize-cl-expression-parser (name)
  (eval
   `(defparser ,name
               ((start exp) $1)
               ((exp :open :2-ary-operator exp exp :close)
                `(,$3 " " ,$2 " " ,$4))
               ((exp :open :function-1 exp :close)
                `(#| ,@(cl-function-name-replace $2) |# ,$2 "(" ,$3 ")"))
               ((exp :open :function-2 exp exp :close)
                `(#| ,@(cl-function-name-replace $2) |# ,$2 "(" ,$3 ", " ,$4 ")"))
               ((exp factor) $1)
               ((factor :var) $1)
               ((factor :constant) $1))))

(defun cl-function-match (tree)
  (if tree
      (cons 
       (process-cl-function-match (car tree))
       (cl-function-match (cdr tree)))))

(defun process-cl-function-match (node)
  (if node
      (if (consp node) 
          (if (or (eql (car node) :function-1) (eql (car node) :function-2))
              (list (car node) (cl-function-name-replace (cadr node)))
            (cons 
             (process-cl-function-match (car node))
             (process-cl-function-match (cdr node))))
        node)))

(defun cl-function-name-replace (node)
  (case node ('abs 'fabs) ('/- "pdiv") (otherwise node)))
                 
(defun get-cl-expression (tree)
  "Answer OpenCL C99 arithmetic expression for parse <tree>."
  (let ((result "")
        (exp (reject (flatten-parenthesis (process-cl-function-match tree))
                     (lambda (o) (keywordp o)))))
    (dolist (i exp)
      (setf result (concatenate 'string result (if (or (numberp i) (symbolp i)) (format nil "~A" i) i))))
    (concatenate 'string (string-downcase result))))

  
