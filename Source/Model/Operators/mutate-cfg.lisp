
(defclass cfg-mutation (subtree-crossover)
  ((production-selection-weight-function 
    :initarg :production-selection-weight-function 
    :accessor production-selection-weight-function)))


(defmethod initialize-properties :after ((o cfg-mutation))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'production-selection-weight-function :label "Production selection weight" 
    :accessor-type 'accessor-accessor-type :data-type 'list :editor 'function-editor)
   (:name 'min-subtree-depth :visible nil)))

(defmethod arity ((o cfg-mutation))
  1)

(defmethod operate ((o cfg-mutation) language expresions)
  (funcall (value-function o) (first expresions) language o))

(defun mutate-cfg (program language operator)
  "Answer a mutated expresion of <program>."
  (let ((weight-function (production-selection-weight-function operator)))
    (directed-crossover-cfg 
     (create-random-from-production language '(start) (max-size language) weight-function)
     program
     language 
     operator)))

(defun random-create-cfg (program language operator)
  (declare (ignore program))
  (let ((weight-function (production-selection-weight-function operator)))
    (create-random-from-production language '(start) (max-size language) weight-function)))

(defun mutate-reuse-cfg (program language operator)
  (let ((weight-function (production-selection-weight-function operator)))
    (directed-crossover-cfg 
     program
     (create-random-from-production language '(start) (max-size language) weight-function)
     language 
     operator)))

(defun mutate-production-cfg (program language operator)
  (mutate-production-cfg-single program language operator))

(defun mutate-production-cfg-single (program language operator)
  (let* ((grammar (grammar language))
         (parse-tree (parse grammar program))
         (index (select-source-index-mutate-production-cfg parse-tree language operator)))
    (if index 
        (let* ((tree-source (select-subtree-cfg parse-tree (1+ index)))
               (tree-source-type (car tree-source))
               (tree-source-value (compress-flatten-parenthesis-token-value tree-source))
               (marker '(:mutate-production-cfg-auxiliary-marker :mutate-production-cfg-auxiliary-marker))
               (new-tree (create-random-from-production 
                          language
                          (list (intern tree-source-type))
                          (tree-size tree-source-value)
                          'lambda-weight-equal-random-selection-list))
               (cutten-tree (compress-flatten-parenthesis-token-value
                             (replace-subtree-cfg parse-tree marker (1+ index)))))
          (replace-marker cutten-tree :mutate-production-cfg-auxiliary-marker new-tree))
      program)))

(defun select-source-index-mutate-production-cfg (parse-tree language operator)
  "Answer source index for cfg-mutation of language on <parse-tree>."
  (select-random-index-cfg
   parse-tree
   (lambda (expresion index)
     (apply (source-selection-function operator) 
            (list expresion index language)))
   language))
