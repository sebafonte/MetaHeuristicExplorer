
(defun crossover (a b language operator)
  "Answer two crossovered individuals with parents <a> and <b>."
  (declare (ignore operator))
  (let ((subtree-a (copy-tree (get-internal-random-subtree a language)))
        (subtree-b (copy-tree (get-internal-random-subtree b language)))
        (insertion-index-a (random (count-internal-subtrees a language)))
        (insertion-index-b (random (count-internal-subtrees b language))))
    (validate-crossover-moves 
     operator
     a (list (replace-internal-subtree a subtree-b (1+ insertion-index-a) language))
     b (list (replace-internal-subtree b subtree-a (1+ insertion-index-b) language))
     (max-depth language)
     (max-size language))))

(defun validate-crossover-moves (operator a child-a b child-b max-depth max-size)
  "Function which validates a crossover move for two new individuals <child-a> and <child-b>, answer
   these if they satisfy size and depth conditions or the original values in <a> and <b>.
   #NOTE: Something similar used John Koza for it's crossover operation."
  (let ((ca (car child-a)) 
        (cb (car child-b)))
    (values             
     (validate-crossover-move-subtree max-depth max-size (min-subtree-depth operator) a child-a)
     (validate-crossover-move-subtree max-depth max-size (min-subtree-depth operator) b child-b))))

(defun validate-crossover-move-subtree (max-depth max-size min-subtree-depth old new)
  "Answer <new> expression if satisfy all conditions, otherwise answer <old> expression."
  (if (or (< (tree-depth new) min-subtree-depth) 
          (> (tree-depth new) max-depth) 
          (> (tree-size new) max-size))
      old
    new))

(defun mutate (exp language operator)
   "This mutation inserts a new random created subtree into an <exp> node while <a> max-depth and max-size
   conditions keep satisfied."
  (let ((new-expression (create-expresion language (max-size language) (random-integer 1 4) t nil)))
    (replace-internal-subtree exp 
                                   new-expression
                                   (1+ (get-random-subtree-index-with-arguments
                                        exp 
                                        (source-selection-function operator)
                                        language
                                        nil
                                        (list new-expression exp)))
                                   language)))

(defun mutate-point (exp language operator)
  "Answer a new subexp mutated in just one node (and just changes node type).
   #NOTE: Implementation note:
           - If exp is a terminal (constant or variable), create a terminal
           - If exp is a symbol (representing a function), change the function"
  (let ((index (1+ (get-random-subtree-index 
                     exp
                     (source-selection-function operator)
                     language
                     t))))
    (replace-subtree 
     exp 
     (mutate-point-element (get-subtree exp index) language)
     index)))

(defun mutate-point-element (element language)
  "Answer a new element as a result of <element> mutation for <language>."
  (cond 
   ;; Terminal node (constant or variable, not a list terminal)
   ((or (node-constant-p element language) 
        (node-language-variable-p element language)) 
    (create-terminal language))
   ;; Terminal node of a list, but not a valid subexp
   ((function-symbol-p element language) 
    (let* ((number-arguments (cadr (assoc element (functions language))))
           (replacements (remove-if (lambda (x) 
                                          (or (not (= (cadr x) number-arguments))
                                              (equal (car x) element)))
                                        (functions language))))
      (if replacements 
          (car (random-element replacements))
        element)))
   ;; Unknown symbol
   (t (error "Unknown symbol."))))
