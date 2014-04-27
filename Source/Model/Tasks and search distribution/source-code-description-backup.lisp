
(defvar *source-code-object-registry* nil)



(defun new-source-description (object)
  "Answer the source code description for <o>."
  (setf *source-code-object-registry* (make-hash-table))
  (let ((body (source-description object))
        (instanciation (instanciation-source-description object)))
    (append (list 'progn) (append instanciation (list body)))))

(defmethod source-description ((o t))
  "Answer the source code description for <o>."
  o)

(defmethod source-description ((o base-model))
  "Answer the source code description for <o>."
  (let ((instance (gethash o *source-code-object-registry*)))
    (if instance
        instance
      (let ((symbol (intern (symbol-name (gensym)))))
        (set-hash *source-code-object-registry* o symbol)
        (append (source-setter-description-from-slots o symbol) 
                (list symbol))))))

(defmethod source-description ((o standard-class))
  "Answer the source code description for <o>."
  (list 'find-class (list 'quote (class-name o))))

(defmethod source-description ((o symbol))
  "Answer the source code description for <o>."
  (if (or (eql o t) (eql o nil))
      o
    (if (keywordp o)
        (list 'internkey (symbol-name o))
      (list 'quote o))))

(defmethod source-description ((o list))
  "Answer the source code description for <o>."
  (let ((lista (list 'list)))
    (dolist (elemento o)
      (appendf lista (list (source-description elemento))))
    lista))

(defmethod source-description ((o array))
  "Answer the source code description for <o>."
  (let ((elements)
        (dimensions (array-dimensions o)))
    ;; 1-dimension arrays
    (when (= (length dimensions) 1)
      (setf elements (list 'make-array (cons 'list dimensions) :initial-contents))
      (let ((inner-elements))
        (dotimes (i (first dimensions))
          (appendf inner-elements (list (source-description (aref o i)))))
        (appendf elements (list (cons 'list inner-elements)))))
    ;; 2-dimension arrays
    (when (= (length dimensions) 2)
      (setf elements (list 'make-array (cons 'list dimensions) :initial-contents))
      (let ((outer-elements))
        (dotimes (i (first dimensions))
          (let ((inner-elements))
            (dotimes (j (second dimensions))
              (appendf inner-elements (list (source-description (aref o i j)))))
            (appendf outer-elements (list (cons 'list inner-elements)))))
        (appendf elements (list (cons 'list outer-elements)))))
    ;; Unsupported array types
    (if (> (length dimensions) 3)
        (error "Arrays with 3 or more dimensions are not supported."))
    ;; Answer result
    elements))

(defmethod source-description ((o string))
  "Answer the source code description for <o>."
  (format nil "~A" o))

(defmethod source-description ((o function))
  "Answer the source code description for <o>."
  (list 'quote (system::function-name o)))

(defmethod source-description ((o generic-function))
  "Answer the source code description for <o>."
  (list 'quote (system::generic-function-name o)))

(defmethod source-description ((o standard-method))
  "Answer the source code description for <o>."
  (list 'quote (second (system::function-name (system::method-function o)))))

(defmethod source-description ((o hash-table))
  "Answer the source code description for <o>."
  (if (> (hash-table-count o) 0)
      (let ((elements)
            (result (list 'let))
            (table-name (intern (gensym))))
        (appendf result 
                 (list (list (list table-name (list 'make-hash-table)))))
        (maphash (lambda (key value)
                   (appendf elements (list (source-description key) 
                                           (source-description value))))
                 o)
        (appendf result 
                 (list (append (list 'set-hash table-name) elements))
                 (list table-name)))
    (list 'make-hash-table)))
  
(defmethod source-description ((o connection-administrator))
  "Answer the source code description for <o>."
  (list 'system-get (quote 'main-connection-administrator)))

;; #TODO: - Check for possible problems
;;        - Check for dependent properties
;;        - Check enought test cases
(defmethod source-setter-description-from-slots ((o base-model) object-identifier)
  "Answer the source code description from slot definition of <o>."
  (let ((slot-definitions (clos:class-slots (class-of o)))
        (argument-list (list 'setf)))
    (dolist (slot-definition slot-definitions)
      (let ((slot-name (clos:slot-definition-name slot-definition)))
        (if (and (not (excluded-slot-name o slot-name))
                 (clos:slot-boundp o slot-name))
            (appendf argument-list 
                     (list (list (intern slot-name) 
                                 object-identifier)
                           (source-description
                            (if (excluded-slot-value o (slot-value o slot-name))
                                nil
                              (slot-value o slot-name))))))))
    (cons 'progn (list argument-list))))

(defmethod excluded-slot-name ((o t) slot-name)
  nil)
    
(defmethod excluded-slot-name ((o object-with-properties) slot-name)
  (equal slot-name 'properties-definition))

(defmethod excluded-slot-name ((o base-pane) slot-name)
  (or (equal slot-name 'properties-definition) 
      (equal slot-name 'property-editors)
      (equal slot-name 'interface-arguments)))

(defmethod excluded-slot-value ((o t) value)
  nil)

(defmethod excluded-slot-value ((o base-pane) value)
  (discard-type value))

(defmethod source-setter-description-from-slots :before ((p pane-explorer) object-identifier)
  (remap-population-interface (interface p)))

(defmethod source-setter-description-from-slots :before ((p pane-buffer) object-identifier)
  (remap-population-pane p))

(defmethod source-setter-description-from-slots :before ((p pane-editor-entity-explorer) object-identifier)
  (remap-population-interface (interface p)))

(defun discard-type (object)
  "Answer whether <object> has to be discarded in the caller source descriptor."
  (let ((class (class-name (class-of object))))
    (or (is-kind-of object 'base-interface)
        (equal class 'mp::time-event)
        (equal class 'mp:process)
        (equal class 'graphics-ports:image))))

(defun instanciation-source-description (object)
  "Answer the instanciation code for objects in *source-code-object-registry*.
   #NOTE: <object> is not used because all information is in *source-code-object-registry*."
  (let ((code))
    (maphash (lambda (key value)
               (appendf code (list (list 'setf value (valid-instance-descriptor key)))))
             *source-code-object-registry*)
    code))

(defmethod valid-instance-descriptor ((o t))
  "Answer a list with <o> instanciation source code."
  (list 'make-instance  
        (source-description (clos:class-name (class-of o)))))

(defmethod valid-instance-descriptor ((o grammar))
  "Answer a list with <o> instanciation source code."
  (list 'make-instance 
        (source-description (clos:class-name (class-of o)))
        :name (source-description 'serialized-valid-grammar)
        :lexer (source-description 'lisp-math-expression-token-type-lexer)
        :parser-initializer (source-description 'initialize-lisp-math-expression-parser)))

(defmethod valid-instance-descriptor ((o base-pane))
  "Answer a list with <o> instanciation source code."
  (list 'make-instance  
        (source-description (clos:class-name (class-of o)))
        :open nil
        :prepare nil))

(defmethod valid-instance-descriptor ((o pane-editor-entity))
  "Answer a list with <o> instanciation source code."
  (list 'make-instance  
        (source-description (clos:class-name (class-of o)))
        :open nil
        :prepare nil
        :interface-arguments
         (source-description
          (list :selected-tab (capi:choice-selection (tab (interface o)))))))

(defmethod valid-instance-descriptor ((o pane-editor-entity-explorer))
  "Answer a list with <o> instanciation source code."
  (list 'make-instance  
        (source-description (clos:class-name (class-of o)))
        :open nil
        :prepare nil))

(defun save-source-description (object file-path &key (print-function 'print) (tag "#source-description"))
  "Save the source code description of <object> into a file with <file-path>.
   64 levels max. of printing and 10 thousand of bytes per file."
  (let ((*print-level* 64)
        (*print-length* 10000000))
    (with-open-file (ostream file-path
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
      (format ostream tag)
      (funcall print-function (new-source-description object) ostream)
      (format ostream "~%"))))

(defmethod transportable-code-description ((o t))
  "Answer a transportable text description for <o>."
  (let ((*print-level* 64)
        (*print-length* 10000000))
    (format nil "~S~%" (new-source-description o))))

