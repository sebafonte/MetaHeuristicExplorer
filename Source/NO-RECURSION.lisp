;; Compress 1: Working on an update to avoid recursion
(defun compress-1 (program language operator)
  "Apply compress subroutine mutation on <program>."
  (declare (ignore operator))
  (if (> (tree-depth program) 1)
      (let ((index (get-random-subtree-index-with-arguments 
                    program 'lambda-subtree-weight-function-2 language)))
        (if index
            (let* ((point (1+ index))
                   (tree (get-internal-subtree program point language))
                   (function-id (internalize-function (manager language) tree)))
              (replace-internal-subtree program (list function-id) point language))
          program))
    program))
  
(defun lambda-subtree-weight-function-2 (list position value)
  (declare (ignore position) (ignore value))
  (if (and (> (tree-size list) 1)
           (not (has-subroutine-call list)))
      1 0))

;; Compress 2: Working on an update to avoid recursion
(defun compress-2 (program language operator)
  "Apply compress subroutine mutation on <program>."
  (if (> (tree-depth program) 1)
      (let* ((point (get-compress-random-point program language))
             (tree (get-internal-subtree program point language))
             (children-points (get-compress-children-points tree operator language))
             (children-trees (get-compress-children-trees tree children-points language))
             (argument-counter 0))
        (dolist (children-point children-points)
          (setf tree (replace-internal-subtree tree (argument-name argument-counter) children-point language))
          (incf argument-counter))
        (let ((function-id (internalize-function-with-arguments (manager language) tree (length children-points))))
          (replace-internal-subtree program (append (list function-id) children-trees) point language)))
    program))

(defun lambda-subtree-extraction-weight-function-2 (list position value max-depth)
  (declare (ignore position))
  (if (and 
       (= (third value) max-depth)
       (> (tree-depth list) 1)
       (not (has-subroutine-call list)))
      1 0))

;; #NOTE: With arguments function version
(defun safe-weight-nodes-with-arguments (expresion function language
                                                           &optional (include-functions nil) (arguments nil))
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
    (if (> sum 0)
        (values 
         (mapcar (lambda (i) (incf last (/ i sum))) 
                 (nreverse selection-indexes))
         t)
      (values
       (nreverse selection-indexes)
       nil))))

(defun get-random-subtree-index-with-arguments (expresion function language &optional (include-functions nil) (arguments nil))
  "Answer the index of a random subtree of <expresion>."
  (block nil
    (let ((value (park-miller-randomizer))
          (index 0))
      (multiple-value-bind (accumulated-values flag)
          (safe-weight-nodes-with-arguments 
           expresion function language include-functions arguments)
        ;; When value >= 1 (maybe cuz low precision) answer the last element
        (when flag
          (cond ((zerop value) 0)
                ((>= value 1) (1- (length accumulated-values)))
                (t (dolist (i accumulated-values)
                     (if (> i value) (return index))
                     (incf index)))))))))
