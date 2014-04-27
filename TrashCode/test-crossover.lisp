
(defun directed-crossover-cfg (program-a program-b language operator)
  "Answer a new expression as crossover of <program-a> and <program-a> with <language> syntax."
  (let* ((grammar (grammar language))
         (parse-tree-a (parse grammar program-a))
         (parse-tree-b (parse grammar program-b)))
    (multiple-value-bind (index-a index-b)
        (select-indexes-cfg parse-tree-a parse-tree-b language operator)
      (if (and index-a index-b)
          (let* ((subtree (copy-tree (select-subtree-cfg parse-tree-a (1+ index-a))))
                 (result-parse-tree (replace-subtree-cfg parse-tree-b subtree (1+ index-b)))
                 (result (compress-flatten-parenthesis-token-value result-parse-tree)))
            ;; #DEBUGGING
            (when (not (equals 'gl-app-draw (car result)))
                (progn 1))
            ;; #END
            result)
        (error "Crossover miss")))))
