
(defun crossover-cfg (program-a program-b language operator)
  "Answer two expresions created with two directed crossover operations."
  (values (directed-crossover-cfg program-a program-b language operator)
          (directed-crossover-cfg program-b program-a language operator)))

(defun directed-crossover-cfg (program-a program-b language operator)
  "Answer a new expression as crossover of <program-a> and <program-a> with <language> syntax."
  (let* ((grammar (grammar language))
         (parse-tree-a (parse grammar program-a))
         (parse-tree-b (parse grammar program-b)))
    (multiple-value-bind (index-a index-b)
        (select-indexes-cfg parse-tree-a parse-tree-b language operator)
      (if (and index-a index-b)
          (let ((subtree (copy-tree (select-subtree-cfg parse-tree-a (1+ index-a)))))
            (deparse
             (replace-subtree-cfg parse-tree-b subtree (1+ index-b))))
        (error "Crossover miss")))))

(defun select-indexes-cfg (parse-tree-a parse-tree-b language operator)
  "Answer a pair of indexes from program-a and program-b or nil.
   #NOTE: nil if it isn't possible to apply a crossover operation."
  (let* ((common-tokens (common-tokens language parse-tree-a parse-tree-b))
         (source-indexes (select-source-indexes-cfg parse-tree-a common-tokens language operator)))
    (block nil
      (dolist (source-index source-indexes)
        (let ((target-index (select-destination-index-cfg 
                             parse-tree-b parse-tree-a common-tokens language source-index operator)))
          (if target-index 
              (return-from nil 
                (values source-index target-index))))))))

(defun select-source-indexes-cfg (parse-tree common-tokens language operator)
  "Answer source index for <parse-tree>."
  (if common-tokens
      (select-random-indexes-cfg
       parse-tree
       (lambda (expresion index)
         (apply (source-selection-function operator) 
                (list expresion index common-tokens language)))
       language)))

(defun select-destination-index-cfg (parse-tree parse-tree-source common-elements language source-index operator)
  "Anwser source index for <parse-tree>."
  (select-random-index-cfg
   parse-tree
   (lambda (expresion index)
     (apply (target-selection-function operator) 
            (list expresion parse-tree-source parse-tree index common-elements language source-index)))
   language))

(defun select-random-index-cfg (expresion function language)
  "Anwser a random index of <expression>."
  (block nil
    (let ((value (park-miller-randomizer))
          (values (accumulated-probability-cfg-or-nil expresion function))
          (index 0))
      (declare (integer index))
      (if (null values) 
          nil
        (cond ((zerop value) 0)
              ((>= value 1) (1- (length values)))
              (t (dolist (i values)
                   (if (> i value) (return index))
                   (incf index))))))))

(defun select-random-indexes-cfg (expresion function language)
  "Anwser source possible sources indexes for <parse-tree>."
  (let* ((value (park-miller-randomizer))
         (values (probability-list-cfg-or-nil expresion function))
         (accumulated (accumulated-list values))
         (result-list))
    (declare (integer index))
    (cond ((zerop value) 0)
          ((>= value 1) (1- (length values)))
          (t (progn 
               (dotimes (time (length values))
                 (block nil
                   (let ((index 0))
                     (dolist (i accumulated)
                       (when (> i value)
                         (appendf result-list (list index))
                         (setf values (remove-and-normalize-list values index)
                               accumulated (accumulated-list values))
                         (return-from nil nil))
                       (incf index)))))
                 result-list)))))

(defun remove-and-normalize-list (list remove-index)
  "Answer a new normalized <list> without element in <remove-index>."
  (let ((sum 0)
        (result))
    ;; Create a new list
    (dotimes (i (length list))
      (appendf result 
               (list 
                (if (= i remove-index)
                    0 
                  (nth i list)))))
    ;; Calculate sum
    (dolist (i result)
      (incf sum i))
    ;; Normalize
    (if (= sum 0)
        (mapcar (lambda (i) 0) result)
      (mapcar (lambda (i) (/ i sum)) result))))

(defun accumulated-probability-cfg (expresion function)
  "Answer the accumulated probability selection list for <expression> with <function>."
  (let ((result)
        (sum 0)
        (last 0)
        (actual-index 0))
    (declare (integer actual-index) (number sum))
    (labels ((accumulated-probability-recursive (sub-expresion function tree-node-p)
               (if (and tree-node-p (consp sub-expresion) (symbolp (car sub-expresion)))
                   (let ((p (apply function (list sub-expresion actual-index))))
                     (incf actual-index)
                     (incf sum p)
                     (push p result)))
               (when (consp sub-expresion)
                 (accumulated-probability-recursive (car sub-expresion) function t)
                 (accumulated-probability-recursive (cdr sub-expresion) function nil))))
      (accumulated-probability-recursive expresion function t))
    (mapcar (lambda (i) (incf last (/ i sum))) 
            (nreverse result))))

(defun probability-list-cfg-or-nil (expresion function)
  (let ((result)
        (sum 0)
        (actual-index 0))
    (declare (integer actual-index) (number sum))
    (labels ((accumulated-probability-recursive (sub-expresion function tree-node-p)
               (if (and tree-node-p (consp sub-expresion) (symbolp (car sub-expresion)))
                   (let ((p (apply function (list sub-expresion actual-index))))
                     (incf actual-index)
                     (incf sum p)
                     (push p result)))
               (when (consp sub-expresion)
                 (accumulated-probability-recursive (car sub-expresion) function t)
                 (accumulated-probability-recursive (cdr sub-expresion) function nil))))
      (accumulated-probability-recursive expresion function t))
    (if (= sum 0)
        nil
      (mapcar (lambda (i) (/ i sum))
              (nreverse result)))))

(defun accumulated-probability-cfg-or-nil (expresion function)
  (let ((result)
        (sum 0)
        (last 0)
        (actual-index 0))
    (declare (integer actual-index) (number sum))
    (labels ((accumulated-probability-recursive (sub-expresion function tree-node-p)
               (if (and tree-node-p (consp sub-expresion) (symbolp (car sub-expresion)))
                   (let ((p (apply function (list sub-expresion actual-index))))
                     (incf actual-index)
                     (incf sum p)
                     (push p result)))
               (when (consp sub-expresion)
                 (accumulated-probability-recursive (car sub-expresion) function t)
                 (accumulated-probability-recursive (cdr sub-expresion) function nil))))
      (accumulated-probability-recursive expresion function t))
    (if (= sum 0)
        nil
      (mapcar (lambda (i) (incf last (/ i sum))) 
              (nreverse result)))))

(defun select-subtree-cfg (tree index)
  (let ((actual-index 0))
    (declare (special actual-index))
    (select-subtree-recursive-cfg tree t index)))

(defun select-subtree-recursive-cfg (tree tree-node-p index)
  (block nil
    (when (and tree-node-p (consp tree) (symbolp (car tree)))  
      (if (= (incf actual-index) index) 
          (return tree)))
    (if (consp tree) 
        (let ((result-a) 
              (result-b))
          (if (setf result-a (select-subtree-recursive-cfg (car tree) t index)) 
              (return result-a))
          (if (setf result-b (select-subtree-recursive-cfg (cdr tree) nil index)) 
              (return result-b))))))

(defun replace-subtree-cfg (tree subtree index)
  (let ((actual-index 0))
    (declare (special actual-index))
    (replace-subtree-cfg-recursive tree subtree index t)))

(defun replace-subtree-cfg-recursive (tree subtree index tree-node-p)
  (block nil
    (when (and tree-node-p (consp tree) (symbolp (car tree)))  
      (if (= (incf actual-index) index) (return subtree)))
    (if (consp tree) 
        (return (cons (replace-subtree-cfg-recursive (car tree) subtree index t)
                      (replace-subtree-cfg-recursive (cdr tree) subtree index nil))))
    tree))

(defun common-tokens (language a b)
  (let ((a (flatten a))
        (b (flatten b)))
    (unique-eql 
     (union 
      (select a (lambda (x) (find-if (lambda (y) (compatible-production language x y)) b)))
      (select b (lambda (x) (find-if (lambda (y) (compatible-production language x y)) a)))))))

(defun compatible-production (language a b)
  "Answer whether production <a> and <b> are compatible for tree exchanges."
  (and (eq a b) (find a (crossover-nodes (grammar language)))))


