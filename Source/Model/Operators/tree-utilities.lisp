;;
;; #TODO: - Use local recursion and optimize
;;        - Avoid special var
;;        - Create another version using lists as placeholders
;;

;;; Subtree selection functions 1
(defun get-internal-random-subtree (tree language)
  "Answer a random subtree of <tree> internal subtrees."
  (get-internal-subtree 
   tree 
   (1+ (random (count-internal-subtrees tree language)))
   language))

(defun get-internal-subtree (tree index language)
  "Answer the subtree on <tree> which are significative as a tree node at <index>."
  (let ((index-current 0))
    (declare (special index-current))
    (get-internal-subtree-recursive tree t index language)))

(defun get-internal-subtree-recursive (tree tree-node-p index language)
  "Answer the subtree on <tree> which are significative as a tree node at <index>."
  (block nil
    ;; #TODO: Replace when by if
    (when (and tree-node-p (not (function-symbol-p tree language)))  
      (if (= (incf index-current) index) (return tree)))
    (if (consp tree) 
        (let ((result-a) 
              (result-b))
          (if (setf result-a (get-internal-subtree-recursive 
                                 (car tree) t index language)) 
              (return result-a))
          (if (setf result-b (get-internal-subtree-recursive 
                                 (cdr tree) nil index language)) 
              (return result-b))))))

;;; Subtree selection functions 2
(defun get-subtree (tree index)
  "Answer the subtree on <tree> at <index>."
  (let ((index-current 0))
    (declare (special index-current))
    (get-subtree-recursive tree t index)))

(defun get-subtree-recursive (tree tree-node-p index)
  "Answer the subtree on <tree> at <index>."
  (block nil
    ;; #TODO: Replace when by if
    (when tree-node-p
      (if (= (incf index-current) index) 
          (return tree)))
    (if (consp tree) 
        (let ((result-a) 
              (result-b))
          (if (setf result-a (get-subtree-recursive (car tree) t index)) 
              (return result-a))
          (if (setf result-b (get-subtree-recursive (cdr tree) nil index)) 
              (return result-b))))))

;;; Subtree counting functions
(defun count-internal-subtrees (tree language)
  "Answer the number of subtrees on <tree> which are significative as a tree node."
  (count-internal-subtrees-recursive tree t language))

(defun count-internal-subtrees-recursive (tree tree-node-p language)
  "Answer the number of subtrees on <tree> which are significative as a tree node."
  (let ((result 0))
    (if (and tree-node-p (not (function-symbol-p tree language))) (setf result 1))
    (when (consp tree) 
        (incf result (count-internal-subtrees-recursive (car tree) t language))
        (incf result (count-internal-subtrees-recursive (cdr tree) nil language)))
    result))

;;; Subtree replacement functions
(defun replace-subtree (tree subtree index)
  "Replaces <subtree> on <tree> at <index>.
   #NOTE: Invalid subtrees could be obtained if <index> it's not correct."
  (let ((index-current 0))
    (labels ((replace-subtree-recursive (tree subtree index tree-node-p)
               (block nil
                 (if (and tree-node-p (= (incf index-current) index))
                     (return subtree))
                 (if (consp tree) 
                     (return (cons (replace-subtree-recursive (car tree) subtree index t)
                                   (replace-subtree-recursive (cdr tree) subtree index nil))))
                 tree)))
      (replace-subtree-recursive tree subtree index t))))

(defun replace-internal-subtree (tree subtree index language)
  "Replaces <subtree> on <tree> at <index>.
   #NOTE: Only nodes which are significative as tree nodes are replaced."
  (let ((index-current 0))
    (declare (special index-current))
    (replace-internal-subtree-recusive tree subtree index t language)))

;;; #TODO: Check if index-current should defined special here too
(defun replace-internal-subtree-recusive (tree subtree index tree-node-p language)
  "Replace <subtree> on <tree> at <index>."
  (block nil
    (when (and tree-node-p (not (function-symbol-p tree language)))
      (if (= (incf index-current) index) 
          (return subtree)))
    (if (consp tree) 
        (return (cons (replace-internal-subtree-recusive (car tree) subtree index t language)
                      (replace-internal-subtree-recusive (cdr tree) subtree index nil language))))
    tree))


#|
;;; #TODO: Check if use this in crossover
(defun subst (old new tree &rest x &key test test-not key)
  (cond ((satisfies-the-test old tree :test test"
                             :test-not test-not :key key)
         new)
        ((atom tree) tree)
        (t (let ((a (apply #'subst old new (car tree) x))
                 (d (apply #'subst old new (cdr tree) x)))
             (if (and (eql a (car tree))
                      (eql d (cdr tree)))
                 tree
               (cons a d))))))                             

(defun replace-subtree (tree subtree index)
  "Replace in <tree> at <index> with <subtree>."
  (let ((contador 0))
    (subst-if subtree 
              #'(lambda (x) (= (incf contador) index))
              tree)))
|#
