
(defclass compression-tree-language (cfg-tree-language)
  ((manager :initarg :manager :initform nil :accessor manager)))


(defmethod initialize-properties :after ((o compression-tree-language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'manager :label "Compression manager" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor 
    :default-value (system-get 'default-compression-subroutine-manager))))

(defmethod initialize-instance :after ((o compression-tree-language) &rest args)
  "Initialize <o>."
  (declare (ignore args))
  (when (grammar o)
    (setf (language (grammar o)) o)))

(defmethod copy ((o compression-tree-language))
  (let ((copy (copy-instance o))
        (new-operators (funcall (ttrav #'cons (lambda (x) (copy x))) (operators o))))
    (setf (operators copy) new-operators
          (language (grammar copy)) copy)
    copy))

(defmethod arity-token ((l compression-tree-language) word)
  (arity-token (manager l) word))

(defmethod arity-value ((l compression-tree-language) word)
  (arity-value (manager l) word))

(defun subroutine-call-node-p (o)
  (and 
   (symbolp o)
   (equals "GP-SUBROUTINE" (package-name (symbol-package o)))))

(defmethod node-variable-p (node (language compression-tree-language))
  "Answer whether <node> represent a variable in <language>."
  (or (subroutine-call-node-p node)
      (node-language-variable-p node language)))

(defmethod node-argument-p (node (language compression-tree-language))
  (and (symbolp node)
       (let ((name (symbol-name node)))
         (and (> (length name) 3)
              (string-equal (string-upcase name) "ARG" 
                            :start1 0 :end1 3 :start2 0 :end2 3)))))

(defmethod function-symbol-p (node (language compression-tree-language)) 
  "Answer whether <node> represent a function in <language>."
  (or 
   (has-subroutine (manager language) node)
   (not (null (assoc node (functions language))))))

(defmethod subroutines-list ((language compression-tree-language))
  (subroutines-list (manager language)))

(defmethod update-grammar-subroutines ((o compression-tree-language))
  "Update grammar productions"
  (set-grammar-tokens o)
  (update-end-productions 
   (grammar o) 
   (tokens (grammar o)) 
   (append (functions o) (subroutine-tokens o))
   (variables o) 
   (specialized-tokens o)))

(defmethod set-grammar-tokens ((o compression-tree-language))
  (setf (tokens (grammar o))
        (append (variable-tokens o)
                (function-tokens o)
                (auxiliar-tokens o)
                (fixed-constants-tokens o)
                (specialized-tokens o)
                (subroutine-tokens o))))

(defmethod subroutine-tokens ((l compression-tree-language))
  (mapcar 
   (lambda (o) (list o :0-ary-operator))
   (subroutines-list l)))

(defmethod can-create-token ((l compression-tree-language) token)
  "Answer whether <language> can create <token> in a new random expression." 
  (or (structural-symbol token)
      (find-if (lambda (value) (equal token (car value))) (functions l))
      (find-if (lambda (value) (equal token (car value))) (subroutine-tokens l))))

(defmethod update-language :after ((o compression-tree-language))
  "Updates <o> internal state."
  (update-grammar-subroutines o))


#|
(defmethod copy-cyclic ((i compression-tree-language) &optional table new-object)
  (cop i))
  
;; #TODO: Delete this and use events for recalculation
;;(defmethod create-random-from-production :before ((o compression-tree-language) terminal max-size weight-function)
;;  (update-grammar-subroutines o))

(defmethod node-variable-p (node (language compression-tree-language))
  "Answer whether <node> represent a variable in <language>."
  (block nil
    ;; Check for subroutine call
    (when (and (symbolp node) (equal "GP-SUBROUTINE" (package-name (symbol-package node))))
      (return t))
    ;; Check for variable nodes
    (dolist (var (variables language))
      (when (eq var node) 
        (return t)))))
|#