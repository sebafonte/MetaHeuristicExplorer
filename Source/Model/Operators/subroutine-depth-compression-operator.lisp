(defclass subroutine-depth-compression-operator (subroutine-compression-operator)
  ((max-depth :initarg :max-depth :accessor max-depth)
   (max-arguments :initarg :max-arguments :accessor max-arguments)))


(defmethod initialize-properties :after ((o subroutine-depth-compression-operator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'max-depth :label "Max subroutine depth" :default-value 3
    :accessor-type 'accessor-accessor-type :data-type 'integer :editor 'integer-editor)
   (:name 'max-arguments :label "Max subroutine arguments" :default-value 3
    :accessor-type 'accessor-accessor-type :data-type 'integer :editor 'integer-editor)))

(defun lisp-math-expression-with-subroutines-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  ;; Check for defined function
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    ;; Check for numeric constant
    (if (equal token-type :unknown)
        (setf token-type (if (numberp word) :constant)))
    ;; Check for subroutine call
    (if (is-subroutine-call-node word)
        (setf token-type (arity-token grammar word)))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun compress-2 (program language operator)
  "Apply compress subroutine mutation on <program>."
  (declare (ignore operator))
  (if (> (tree-depth program) 1)
      (let* ((point (get-compress-random-point program language))
             (tree (get-internal-subtree program point language))
             (original-tree (copy-tree tree))
             (children-points (get-compress-children-points tree operator language))
             (children-trees (get-compress-children-trees tree children-points language))
             (argument-counter 0))
        (dolist (children-point children-points)
          (setf tree (replace-internal-subtree tree (argument-name argument-counter) children-point language))
          (incf argument-counter))
        (let ((function-id (internalize-function-with-arguments (manager language) tree (length children-points))))
          (replace-internal-subtree program (append (list function-id) children-trees) point language)))
    program))

(defun get-compress-random-point (program language)
  (1+ (get-random-subtree-index-with-arguments 
       program 
       'lambda-subtree-extraction-weight-function-1 
       language)))

(defun get-compress-children-points (tree operator language)
  (mapcar 
   (lambda (x) (1+ x))
   (get-random-subtree-indexes-with-arguments 
    tree
    (lambda (list position value)
      (lambda-subtree-extraction-weight-function-2 list position value (max-depth operator)))
    language
    (max-arguments operator))))

(defun get-compress-children-trees (tree children-points language)
  (mapcar (lambda (children-point)
            (get-internal-subtree tree children-point language))
          children-points))

(defun lambda-subtree-extraction-weight-function-1 (list position value)
  (declare (ignore position) (ignore value))
  (if (> (tree-depth list) 1) 1 0))

(defun lambda-subtree-extraction-weight-function-2 (list position value max-depth)
  (declare (ignore position))
  (if (and 
       (= (third value) max-depth)
       (> (tree-depth list) 1))
      1 0))

;; #TODO: Mover al core
(defun get-random-subtree-indexes-with-arguments
       (expresion function language n &optional (intermediate-nodes nil) (args nil))
  (let ((values-list (weight-nodes-with-arguments-with-depth
                      expresion function language intermediate-nodes args))
        (list))
    (dotimes (i n)
      (let ((index (select-roulette-wheel (accumulated-list (normalize-list values-list)))))
        (when index
          (appendf list (list index))
          (setf (nth index values-list) 0))))
    list))
 
(defun select-roulette-wheel (list)
  (let ((value (park-miller-randomizer)) 
        (index 0))
    (cond ((zerop value) 0)
          ((>= value 1) (1- (length list)))
          (t (dolist (i list)
               (if (> i value) (return index))
               (incf index))))))

(defun weight-nodes-with-arguments-with-depth
       (expresion function language &optional (include-functions nil) (arguments nil))
  (let ((selection-indexes)
        (sum 0)
        (last 0)
        (index-current 0)
        (index-global 0))
    (labels ((weight-nodes-recursive (expresion function tree-node-p arguments depth)
               ;; Check whether to visit current node
               (if (and tree-node-p 
                        (or include-functions
                            (not (function-symbol-p expresion language))))
                   (let ((p (apply function (list expresion index-current (list language arguments depth)))))
                     (incf index-current)
                     (incf sum p)
                     (push p selection-indexes)))
               (incf index-global)
               ;; Go through CAR and CDR
               (when (consp expresion)
                 (weight-nodes-recursive (car expresion) function t arguments (1+ depth))
                 (weight-nodes-recursive (cdr expresion) function nil arguments depth))))
      (weight-nodes-recursive expresion function t arguments 1))
    (nreverse selection-indexes)))

(defmethod arity-token ((m compression-subroutine-manager) word)
 (ecase (arity-value m word)
   (0 :0-ary-operator)
   (1 :1-ary-operator)
   (2 :2-ary-operator)
   (3 :3-ary-operator)
   (4 :4-ary-operator)
   (5 :5-ary-operator)
   (6 :6-ary-operator)
   (7 :7-ary-operator)
   (8 :8-ary-operator)
   (9 :9-ary-operator)
   (10 :10-ary-operator)))

(defmethod arity-value ((m compression-subroutine-manager) word)
  (second (find-if 
           (lambda (o) (eql (car o) word))
           (hash-values (subroutines m)))))
