
(defclass subroutine-compression-operator (unary-genetic-operator)
  ((value-function :initarg :value-function :accessor value-function)
   (selection-function :initarg :selection-function :accessor selection-function)))


(defmethod initialize-properties :after ((object subroutine-compression-operator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'value-function :label "Mutation function" :accessor-type 'accessor-accessor-type
    :data-type 'list :default-value 1 :editor 'list-editor
    :possible-values (compression-value-functions))))

(defun compress-1 (program language operator)
  "Apply compress subroutine mutation on <program>."
  (declare (ignore operator))
  (if (> (tree-depth program) 1)
      (let* ((point (1+ (get-random-subtree-index-with-arguments 
                         program 'lambda-subtree-weight-function language)))
             (tree (get-internal-subtree program point language))
             (function-id (internalize-function (manager language) tree)))
        (replace-internal-subtree program (list function-id) point language))
    program))
  
(defun lambda-subtree-weight-function (list position value)
  (declare (ignore position) (ignore value))
  (if (> (tree-size list) 1) 1 0))

(defun expand-1 (program language operator)
  "Apply expansion-1 operation on <program> using subroutines of <language>."
  (declare (ignore operator))
  (let* ((elements (flatten program))
         (function-calls (select elements 'subroutine-call-node-p)))
    (dolist (function-call function-calls)
      (let ((subtree (subtree-for (manager language) function-call)))
        (setf program 
              (if (> (arity-value (manager language) function-call) 0)
                  (replace-subtree-with-arguments-local program function-call subtree)
                (replace-subtree-local program function-call subtree)))))
    program))

(defun replace-subtree-with-arguments-local (tree id subtree)
  (if (consp tree)
      (if (equals (car tree) id)
          (replace-subtree-argument-bindings tree subtree)
        (cons (replace-subtree-with-arguments-local (car tree) id subtree)
              (replace-subtree-with-arguments-local (cdr tree) id subtree)))
    tree))

(defun replace-subtree-argument-bindings (tree subtree)
  (let ((new-tree subtree)
        (argument-index 0))
    (dolist (argument-value (cdr tree))
      (let ((argument-id (intern (format nil "ARG~A" argument-index))))
        (setf new-tree (replace-subtree-arguments new-tree argument-id argument-value))
        (incf argument-index)))
    new-tree))

;; #TODO: Move to utilities
(defun replace-subtree-arguments (tree id subtree)
  (if (consp tree)
      (cons (replace-subtree-arguments (car tree) id subtree)
            (replace-subtree-arguments (cdr tree) id subtree))
    (if (equals tree id)
        subtree
      tree)))

(defun expand-all (program language operator)
  "Apply expansion-all operation on <program> using subroutines of <language>."
  (declare (ignore operator))
  (let ((elements)
        (function-calls t))
    (while-do function-calls
     (setf elements (flatten program)
           function-calls (select elements 'subroutine-call-node-p))
     (dolist (function-call function-calls)
       (let ((subtree (subtree-for (manager language) function-call)))
         (setf program (replace-subtree program function-call subtree)))))
    program))

(defun shake-1 (program language operator)
  "Apply shake mutation on <program> using subroutines un <language>."
  (declare (ignore operator))
  (shake-subroutine-calls language program))

(defun shake-subroutine-calls (language program)
  (if (consp program)
      (cons 
       (shake-subroutine-calls language (car program))
       (shake-subroutine-calls language (cdr program)))
    (if (subroutine-call-node language program)
        (shake-subroutine-call-node language program)
      program)))

;; #TODO: Move to utilities
(defun replace-subtree-local (tree id subtree)
  (if (consp tree)
      (if (equals (car tree) id)
          subtree
        (cons (replace-subtree-local (car tree) id subtree)
              (replace-subtree-local (cdr tree) id subtree)))
    tree))

(defun compression-value-functions ()
  '(compress-1 compress-2 expand-1))

