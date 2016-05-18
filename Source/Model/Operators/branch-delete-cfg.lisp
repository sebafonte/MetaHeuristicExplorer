(defclass branch-delete-cfg (cfg-mutation)
  ())


(defun mutate-branch-delete-cfg (program-a program-b language operator)
  "Answer a mutated expresion of <program-a>."
  (values 
   (branch-delete-single-cfg program-a language operator)
   (branch-delete-single-cfg program-b language operator)))      

(defun branch-delete-single-cfg (program language operator)
  (let* ((grammar (grammar language))
         (parse-tree (parse grammar program))
         (index (select-source-index-branch-delete-cfg parse-tree language operator)))
    (if index 
        (let* ((tree-source (select-subtree-cfg parse-tree (1+ index)))
               (tree-source-type (car tree-source))
               (tree-source-value (deparse tree-source))
               (marker '(:branch-delete-cfg-auxiliary-marker :branch-delete-cfg-auxiliary-marker))
               (new-tree (create-random-from-production 
                          language
                          (list (intern tree-source-type))
                          (1- (tree-size tree-source-value))
                          'lambda-weight-equal-random-selection-list))
               (cutten-tree (deparse (replace-subtree-cfg parse-tree marker (1+ index)))))
          (replace-marker cutten-tree :branch-delete-cfg-auxiliary-marker new-tree))
      program)))

(defun select-source-index-branch-delete-cfg (parse-tree language operator)
  "Answer a selected subtree index from <parse-tree> for <operator>."
  (select-random-index-cfg
   parse-tree
   (lambda (expresion index)
     (apply (source-selection-function operator) 
            (list expresion index language)))
   language))

