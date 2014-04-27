
 
(defun feasible-size-depth-fix-check (program language parent-a parent-b)
  (declare (ignore parent-b))
  (if (or (> (tree-depth program) (max-depth language))
          (> (tree-size program) (max-size language)))
      (progn
        (error "Crossover miss A")
        parent-a)
    program))

;; #POINT 1
(defun directed-crossover-cfg (program-a program-b language operator)
  "Answer a new expression as crossover of <program-a> and <program-a> with <language> syntax."
  (let* ((grammar (grammar language))
         (parse-tree-a (parse grammar program-a))
         (parse-tree-b (parse grammar program-b)))
    (multiple-value-bind (index-a index-b)
        (select-indexes-cfg parse-tree-a parse-tree-b language operator)
      (if (and index-a index-b)
          (let ((subtree (copy-tree (select-subtree-cfg parse-tree-a (1+ index-a)))))
            (feasible-size-depth-fix-check
             (compress-flatten-parenthesis-token-value
              (replace-subtree-cfg parse-tree-b subtree (1+ index-b)))
             language
             program-a
             program-b))
        (register-non-possible-crossover-error program-a program-b)))))

(defun register-non-possible-crossover-error (program-a program-b)
  (declare (ignore program-b))
  (error "Crossover miss B")
  program-a)
 