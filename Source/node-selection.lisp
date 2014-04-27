
(defun weight-nodes-print (expresion function language &optional (include-functions nil))
  "Answer node probability selection list for <expression> in order, using <function> for probability assignment.
   #NOTE: Slow print version."
  (let ((selection-subtrees)
        (selection-indexes)
        (index 0)
        (sum 0))
    (labels ((weight-nodes-recursive (expresion function tree-node-p)
               ;; Conditions
               (format t "Received  ~A ~A~%" expresion index)
               (if (and tree-node-p 
                        (or include-functions 
                            (not (function-symbol-p expresion language))))
                   (let ((p (apply function (list expresion index-current))))
                     (format t "Put  ~A ~A~%" expresion index)
                     (incf sum p) 
                     (push (cons expresion p) selection-subtrees)
                     (push (cons index p) selection-indexes)
                     (incf index)))
               ;; Go thru CAR and CDR
               (when (consp expresion)
                 (weight-nodes-recursive (car expresion) function t)
                 (weight-nodes-recursive (cdr expresion) function nil))))
      ;; Recursive function call
      (weight-nodes-recursive expresion function t))
    ;; Process results (reverse)
    (dolist (i selection-subtrees) (setf (cdr i) (/- (cdr i) sum)))
    (dolist (i selection-indexes) (setf (cdr i) (/- (cdr i) sum)))
    (values (reverse selection-subtrees)
            (reverse selection-indexes))))

(defun weight-nodes (expresion function language &optional (include-functions nil))
  "Answer node probability selection list for <expression> in order, using <function> for probability assignment."
  (let ((selection-indexes)
        (sum 0)
        (last 0)
        (index-current 0)
        (index-global 0))
    (labels ((weight-nodes-recursive (expresion function tree-node-p)
               ;; Conditions
               (if (and tree-node-p 
                        (or include-functions
                            (not (function-symbol-p expresion language))))
                   (let ((p (apply function (list expresion index-current))))
                     (incf index-current)
                     (incf sum p)
                     (push p selection-indexes)))
               (incf index-global)
               ;; Go thru CAR and CDR
               (when (consp expresion)
                 (weight-nodes-recursive (car expresion) function t)
                 (weight-nodes-recursive (cdr expresion) function nil))))
      (weight-nodes-recursive expresion function t))
    (mapcar (lambda (i) (incf last (/ i sum))) 
            (nreverse selection-indexes))))

(defun get-random-subtree-index (expresion function language &optional (include-functions nil))
  "Answer the index for a random <expression> subtree."
  (block nil
    (let ((value (park-miller-randomizer))
          (cumulative-list (weight-nodes expresion function language include-functions))
          (index 0))
      ;; When value >= 1 (maybe cuz low precision) answer the last element
      (cond ((zerop value) 0)
            ((>= value 1) (1- (length cumulative-list)))
            (t (dolist (i cumulative-list)
                 (if (> i value) (return index))
                 (incf index)))))))

;;; #NOTE: Nodes weight functions for selection with arguments
(defun weight-nodes-with-arguments (expresion function language &optional (include-functions nil) (arguments nil))
  "Answer node probability selection list for <expression> in order, using <function> for probability assignment.
  #NOTE: With arguments function version."
  (let ((selection-indexes)
        (sum 0)
        (last 0)
        (index-current 0)
        (index-global 0))
    (labels ((weight-nodes-recursive (expresion function tree-node-p arguments)
               ;; Conditions
               (if (and tree-node-p 
                        (or include-functions
                            (not (function-symbol-p expresion language))))
                   (let ((p (apply function (list expresion index-current (list language arguments)))))
                     (incf index-current)
                     (incf sum p)
                     (push p selection-indexes)))
               (incf index-global)
               ;; Go thru CAR and CDR
               (when (consp expresion)
                 (weight-nodes-recursive (car expresion) function t arguments)
                 (weight-nodes-recursive (cdr expresion) function nil arguments))))
      (weight-nodes-recursive expresion function t arguments))
    (mapcar (lambda (i) (incf last (/ i sum))) 
            (nreverse selection-indexes))))

(defun get-random-subtree-index-with-arguments (expresion function language &optional (include-functions nil) (arguments nil))
  "Answer the index for a random <expression> subtree.
  #NOTE: With arguments function version."
  (block nil
    (let ((value (park-miller-randomizer))
          (cumulative-list (weight-nodes-with-arguments 
                            expresion function language include-functions arguments))
          (index 0))
      ;; When value >= 1 (maybe cuz low precision) answer the last element
      (cond ((zerop value) 0)
            ((>= value 1) (1- (length cumulative-list)))
            (t (dolist (i cumulative-list)
                 (if (> i value) (return index))
                 (incf index)))))))

(defun node-selection-function-constant (subtree index)
  "Answer the probability to select <subtree>."
  (declare (ignore subtree) (ignore index))
  1)

(defun node-selection-function-subtrees (subtree index)
  "Answer the probability to select <subtree>."
  (declare (ignore index))
  (if (> (tree-depth subtree) 1) 1 0))

(defun node-selection-function-terminals (subtree index)
  "Answer the probability to select <subtree>."
  (declare (ignore index))
  (if (atom subtree) 1 0))

(defun node-selection-function-depth-proportional (subtree index)
  "Answer the probability to select <subtree>."
  (declare (ignore index))
  (tree-depth subtree))

(defun node-selection-function-size-proportional (subtree index)
  "Answer the probability to select <subtree>."
  (declare (ignore index))
  (tree-size subtree))

;; #TODO: Make max-depth condition work
(defun node-selection-function-size-with-subexp (subtree new-expression algorithm expresion)
  "Answer the probability to select <subtree>.
  #NOTE: Select nodes where replacing new-expression, result will not exceed <algorithm> max-depth/max-size."
  (declare (ignore index))
  (if (and (<= (+ (tree-size new-expression) (tree-size expresion) (- (tree-size subtree)))
               (max-size algorithm))
           (not (function-symbol-p subtree algorithm))
           t)
      1 
    0))

