;;; SIN LIMITACION DE MAX-DEPTH (solo en source)
(defun crossover-cfg-source-selection (parse-tree index common-elements algorithm)
  (let ((subtree-type (car parse-tree)))
    (if (and 
         ;; Constraint to satisfy grammar
         (find subtree-type common-elements :test (lambda (a b) (compatible-production algorithm a b)))
         ;; Gen depth and size constraint
         (let ((subtree (deparse parse-tree)))
           (and (<= (tree-size subtree) (max-size algorithm))
                (<= (tree-depth subtree) (max-depth algorithm)))))
    1 0)))

;;; SIN LIMITACIÓN DE DEPTH EN DESTINO
(defun crossover-cfg-target-selection
       (parse-subtree-target parse-tree-source parse-tree-target index common-elements algorithm source-index)
  (let* ((subtree-target-type (car parse-subtree-target))
         (subtree-target-value (deparse parse-subtree-target))
         (tree-target-value (deparse parse-tree-target))
         (subtree-source (select-subtree-cfg parse-tree-source (1+ source-index)))
         (subtree-source-type (car subtree-source))
         (subtree-source-value (deparse subtree-source)))
    (if (and 
         ;; Gen size constraint
         (<= (- (+ (tree-size subtree-source-value) (tree-size tree-target-value))
                (tree-size subtree-target-value)) 
             (max-size algorithm))
         ;; Constraint to satisfy grammar
         (compatible-production algorithm subtree-target-type subtree-source-type))
        1 0)))

;;; CON LIMITACION DE MAX-DEPTH POR UNO MAS CHICO SIEMPRE (restriccion)
(defun crossover-cfg-target-selection-weight-depth
       (parse-subtree-target parse-tree-source parse-tree-target index common-elements algorithm source-index)
  (let* ((subtree-target-type (car parse-subtree-target))
         (subtree-target-value (deparse parse-subtree-target))
         (tree-target-value (deparse parse-tree-target))
         (subtree-source (select-subtree-cfg parse-tree-source (1+ source-index)))
         (subtree-source-type (car subtree-source))
         (subtree-source-value (deparse subtree-source)))
    (if (and 
         ;; Gen depth and size constraint
         (<= (- (+ (tree-size subtree-source-value) (tree-size tree-target-value)) 
                (tree-size subtree-target-value)) 
             (max-size algorithm))
         ;; Constraint to satisfy grammar
         (compatible-production algorithm subtree-target-type subtree-source-type))
        (if (<= (tree-depth subtree-source-value) (tree-depth subtree-target-value))
            1 0.5)
      0)))

;;; CON LIMITACION DE MAX-DEPTH 
(defun crossover-cfg-target-selection-depth
       (parse-subtree-target parse-tree-source parse-tree-target index common-elements algorithm source-index)
  (let* ((subtree-target-type (car parse-subtree-target))
         (subtree-target-value (deparse parse-subtree-target))
         (tree-target-value (deparse parse-tree-target))
         (subtree-source (select-subtree-cfg parse-tree-source (1+ source-index)))
         (subtree-source-type (car subtree-source))
         (subtree-source-value (deparse subtree-source)))
    (if (and 
         ;; Gen depth and size constraint
         (<= (- (+ (tree-size subtree-source-value) (tree-size tree-target-value))
                (tree-size subtree-target-value)) 
             (max-size algorithm))
         (<= (tree-depth subtree-source-value) (tree-depth subtree-target-value))
         ;; Constraint to satisfy grammar
         (compatible-production algorithm subtree-target-type subtree-source-type))
        1 0)))

;;; SIN LIMITACION DE MAX-DEPTH (solo en source)
(defun branch-delete-cfg-source-selection (parse-tree index algorithm)
  (let* ((subtree-type (car parse-tree))
         (subtree-source (deparse parse-tree))
         (subtree-size (tree-size subtree-source))
         (minimum-size (gethash (intern subtree-type) (minimum-production-sizes (grammar algorithm)))))
    (if (and minimum-size 
             (< minimum-size subtree-size))
        1 0)))

;;; GENERAL NODE SELECTION (all nodes)
(defun mutate-production-cfg-source-selection (parse-tree index algorithm)
  (declare (ignore parse-tree index algorithm))
  1)


(defun tree-depth-cfg (tree index algorithm)
  (let ((actual-index 0)
        (depth-index 1))
    (declare (special actual-index) (special depth-index))
    (tree-depth-cfg-recursive tree t index algorithm)))

(defun tree-depth-cfg-recursive (tree tree-node-p index algorithm)
  (block nil
    (when (and tree-node-p (consp tree) (symbolp (car tree)))  
      (if (= (incf actual-index) index) 
          (return tree)))
    (if (consp tree) 
        (let ((result-a) 
              (result-b))
          (if (setf result-a (select-subtree-recursive-cfg (car tree) t index algorithm)) 
              (return result-a))
          (if (setf result-b (select-subtree-recursive-cfg (cdr tree) nil index algorithm)) 
              (return result-b))))))


(defun cfg-source-selection-functions ()
  (list 
   'crossover-cfg-source-selection))

(defun cfg-target-selection-functions ()
  (list 
   'crossover-cfg-target-selection
   'crossover-cfg-target-selection-depth
   'crossover-cfg-target-selection-min-depth))
