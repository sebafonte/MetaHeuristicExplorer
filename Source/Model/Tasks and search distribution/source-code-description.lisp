
(defparameter *list-argument-size-limit* 2047)
(defvar *source-code-object-registry* nil)


(defun new-source-description (object)
  "Answer <object> source code description from a new registry."
  (setf *source-code-object-registry* (make-hash-table))
  (let ((body (source-description object))
        (instanciation (instanciation-source-description object)))
    (append (list 'progn) (append instanciation (list body)))))

(defmethod source-description ((o t))
  "Answer <o> source code description."
  o)

(defmethod source-description ((o base-model))
  "Answer <object> source code description."
  (let ((instance (gethash o *source-code-object-registry*)))
    (if instance
        instance
      (let ((symbol (intern (symbol-name (gensym)))))
        (set-hash *source-code-object-registry* o symbol)
        (append (source-setter-description-from-slots o symbol) 
                (list symbol))))))

(defmethod source-description ((o standard-class))
  "Answer <object> source code description."
  (list 'find-class (list 'quote (class-name o))))

(defmethod source-description ((o symbol))
  "Answer <object> source code description."
  (if (or (eql o t) (eql o nil))
      o
    (if (keywordp o)
        (list 'internkey (symbol-name o))
      (list 'quote o))))

(defmethod source-description ((o list))
  "Answer <object> source code description."
  (if (< (length o) *list-argument-size-limit*)
      (small-list-source-description o)
    (large-list-source-description o)))

(defun small-list-source-description (o)
  "Answer source code description for <o> with no prevention for #'list function max arguments."
  (let ((list (list 'list)))
    (dolist (element o)
      (appendf list (list (source-description element))))
    list))

(defun large-list-source-description (list)
  "Answer the source code description for <o> with prevention for #list function max arguments."
  (let ((sublists (to-n-paired-list list *list-argument-size-limit*))
        (source (list 'append))
        (sublist-source))
     (dolist (sublist sublists)
       (setf sublist-source nil)
       (dolist (i sublist)
         (appendf sublist-source (list (source-description i))))
       (appendf source (list (append (list 'list) sublist-source))))
     source))

(defmethod source-description ((o array))
  "Answer <o> source code description."
  (let ((dimensions (array-dimensions o)))
    (append (list 'make-array-from-data (cons 'list dimensions))
            (list (cons 'list (get-array-data o))))))

(defun get-array-data (array)
  "Answer a list with <array> data as major row elements."
  (let* ((dimensions (array-dimensions array))
         (max-value (apply '* dimensions))
         (result))
    (dotimes (i max-value)
      (appendf result (list (source-description (row-major-aref array i)))))
    result))

(defun set-array-from-data (array data)
  "Set values into <array> using major row elements on <data>."
  (let ((index 0))
    (dolist (element data)
      (setf (row-major-aref array index) element)
      (incf index))
    array))

(defun make-array-from-data (dimensions data)
  "Answer a new array with <dimensions> and <data>."
  (let ((array (make-array dimensions)))
    (set-array-from-data array data)
    array))

(defmethod source-description ((o string))
  "Answer <o> source code description."
  (format nil "~A" o))

(defmethod source-description ((o function))
  "Answer <o> source code description."
  (list 'quote (system::function-name o)))

(defmethod source-description ((o generic-function))
  "Answer <o> source code description."
  (list 'quote (system::generic-function-name o)))

(defmethod source-description ((o standard-method))
  "Answer <o> source code description."
  (list 'quote (second (system::function-name (system::method-function o)))))

(defmethod source-description ((o hash-table))
  "Answer <o> source code description."
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
  "Answer <o> source code description."
  (list 'system-get (quote 'main-connection-administrator)))

;; #TODO: Add test cases
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
        (eql class 'mp::time-event)
        (eql class 'mp:process)
        (eql class 'graphics-ports:image))))

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
        :name (source-description 'valid-grammar)
        :parser-initializer (source-description 'initialize-null-parser)))

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

(defmethod valid-instance-descriptor ((o log-data))
  "Answer a list with <o> instanciation source code."
  (list 'make-log-data))

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

;; #HACK OPTIMIZATION HACKS
(defmethod excluded-slot-name ((o search-task) slot-name)
  "Answer <o> slot names which are going to be excluded from source description."
  (or (equal slot-name 'properties-definition)
      (equal slot-name 'log-data)
      (equal slot-name 'process)))

(defmethod excluded-slot-name ((o t) slot-name)
  "Answer <o> slot names which are going to be excluded from source description."
  nil)
    
(defmethod excluded-slot-name ((o object-with-properties) slot-name)
  "Answer <o> slot names which are going to be excluded from source description."
  (equal slot-name 'properties-definition))

(defmethod excluded-slot-name ((o base-pane) slot-name)
  "Answer <o> slot names which are going to be excluded from source description."
  (or (equal slot-name 'property-editors)
      (equal slot-name 'interface-arguments)
      (equal slot-name 'properties-definition)))

(defmethod excluded-slot-value ((o t) value)
  "Answer <o> slot names which are going to be excluded from source description."
  nil)

(defmethod excluded-slot-value ((o base-pane) value)
  "Answer <o> slot names which are going to be excluded from source description."
  (discard-type value))


;; #FIX: Compensate a little the excess in amount of arguments
(defmethod source-description ((o array))
  "Answer <o> source code description."
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
    ;; Others
    (when (>= (length dimensions) 3)
        (setf elements (append (list 'make-array-from-data (cons 'list dimensions))
                               (list (cons 'list (get-array-data o))))))
    ;; Answer result
    elements))


#|
;;; #TODO: This seems to have problems with larger objects
(defun instanciation-source-description (object)
  "Answer the instanciation code for objects in *source-code-object-registry*.
   #NOTE: <object> is not used because all information is in *source-code-object-registry*."
  (let ((code (list 'setf)))
    (maphash (lambda (key value)
               (appendf code (list value (valid-instance-descriptor key))))
             *source-code-object-registry*)
    (list code)))

;;; Test performance
(let ((grande))
  (dotimes (i 10000)
    (appendf grande (list i)))
  (setf gg (make-array (length grande) :initial-contents grande)))

(time (progn (source-description gg) nil))
|#