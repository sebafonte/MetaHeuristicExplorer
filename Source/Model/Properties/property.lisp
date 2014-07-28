
(defclass property (base-model)
  ((name :initarg :name :initform nil :accessor name)
   (label :initarg :label :initform "" :accessor label)
   (subject :initarg :subject :initform nil :accessor subject)
   (accessor-type :initarg :accessor-type :initform nil :accessor accessor-type)
   (data-type :initarg :data-type :initform nil :accessor data-type)
   (min-value :initarg :min-value :initform nil :accessor min-value)
   (max-value :initarg :max-value :initform nil :accessor max-value)
   (default-value :initarg :default-value :initform nil :accessor default-value)
   (editor :initarg :editor :initform nil :accessor editor)
   (read-only :initarg :read-only :initform nil :accessor read-only)
   (validation-function :initarg :validation-function :initform nil :accessor validation-function)
   (getter :initarg :getter :initform nil :accessor getter)
   (setter :initarg :setter :initform nil :accessor setter)
   (possible-values :initarg :possible-values :initform nil :accessor possible-values)
   (visible :initarg :visible :initform t :accessor visible)
   (update-callback :initarg :update-callback :initform nil :accessor update-callback)
   (compiled-valuable :initarg :compiled-valuable :initform nil :accessor compiled-valuable)
   (category :initarg :category :initform nil :accessor category)
   (object-parameter :initarg :object-parameter :initform nil :accessor object-parameter)
   (dependency :initarg :dependency :initform nil :accessor dependency)
   (default-value-function :initarg :default-value-function :accessor default-value-function)
   (possible-values-function :initarg :possible-values-function :accessor possible-values-function)))


(defmethod equals ((a property) (b property))
  "Answers whether <a> and <b> are equal."
  (equal (name a) (name b)))

(defmethod add-property ((o object-with-properties) (p property))
  "Adds a property to <o>."
  (setf (properties-definition o) (remove p (properties-definition o) :test 'equals)
        (properties-definition o) (append (properties-definition o) (list p))))

(defmethod possible-accessor-types ()
  "Answers all posible accessor type for properties."
  '(valuable-accessor-type
    accessor-accessor-type
    property-accessor-type))

(defmethod add-property-from-values ((o base-model) 
                                     &key name label accessor-type data-type min-value max-value possible-values 
                                     default-value editor read-only validation-function getter setter 
                                     (visible t) (update-callback nil) (category nil) (object-parameter nil) 
                                     (subject o) (dependency nil)
                                     (default-value-function nil) (possible-values-function nil))
  "Add a property to <o> created with values given as keyword arguments."
  (add-property o (make-instance 'property 
                                 :name name 
                                 :label label
                                 :accessor-type accessor-type 
                                 :data-type data-type
                                 :min-value min-value
                                 :max-value max-value 
                                 :possible-values possible-values
                                 :default-value default-value 
                                 :editor editor 
                                 :read-only read-only 
                                 :validation-function validation-function 
                                 :getter getter
                                 :setter setter
                                 :update-callback update-callback
                                 :visible visible
                                 :category category
                                 :object-parameter object-parameter
                                 :subject (if (null subject) o subject)
                                 :dependency dependency
                                 :default-value-function default-value-function
                                 :possible-values-function possible-values-function)))

(defmethod property-from-values ((o base-model) 
                                 &key name label accessor-type data-type min-value max-value possible-values 
                                 default-value editor read-only validation-function getter setter 
                                 (visible t) (update-callback nil) (category nil) (object-parameter nil) (subject o) (dependency nil)
                                 (default-value-function nil) (possible-values-function nil))
  "Answer a property created with values given as keyword arguments."
  (make-instance 'property 
                 :name name 
                 :label label
                 :accessor-type accessor-type 
                 :data-type data-type
                 :min-value min-value
                 :max-value max-value 
                 :possible-values possible-values
                 :default-value default-value 
                 :editor editor 
                 :read-only read-only 
                 :validation-function validation-function 
                 :getter getter
                 :setter setter
                 :update-callback update-callback
                 :visible visible
                 :category category
                 :object-parameter object-parameter
                 :subject (if (null subject) o subject)
                 :dependency dependency
                 :default-value-function default-value-function
                 :possible-values-function possible-values-function))

(defmethod get-value-for-property ((o base-model) (p property))
  "Answers the value for <p> at an <o>."
  (get-value-for-property-named o (name p)))

(defmethod set-value-for-property ((o base-model) (p property) value)
  "Sets the value for <p> at an <o>."
  (set-value-for-property-named o (name p) value))

(defmethod set-default-property-values ((o base-model))
  "Sets the default value for a property in <o>."
  (dolist (i (non-dependent-properties o))
    (if (not (read-only i))
        (set-default-value-for-property-named o (name i) (default-value i)))))

(defmethod set-default-property-value ((o base-model) (p property))
  "Sets the default value for a property in o."
  (when (not (read-only p))
    (set-value-for-property-named o (name p) nil)
    (set-default-value-for-property-named o (name p) (default-value p))))

(defmacro add-properties-from-values (object &rest args)
  "Add properties specified in <args> to <object>."
  `(progn ,@(loop for a in args collect 
                  `(add-property-from-values ,object ,@a))))

(defmacro property-from-values-list (object &rest args)
  `(list ,@(loop for a in args collect 
                 `(property-from-values ,object ,@a))))

(defmethod clean-properties-definitions ((o base-model))
  "Cleans <o> properties."
  (setf (properties-definition o) nil))

(defmethod property-named ((o base-model) name)
  "Answers <o> property named <label>."
  (find-if (lambda (p) (equal (name p) name)) (properties o)))

(defmethod property-labeled ((o base-model) label)
  "Answers <o> property labeled <label>."
  (find-if (lambda (p) (equal (label p) label)) (properties o)))

(defmethod create-editor-for-property-and-object ((p property) (o base-model))
  "Answer a new editor for <p>, <o>."
  (let ((editor (create-editor-for-editor-class (editor p) p o)))
    (setf (property editor) p)
    editor))

(defmethod compiled-valuable ((p property))
  "Answer a compiled function to evaluate <p>."
  (if (or (not (slot-value p 'compiled-valuable))
          (numberp (slot-value p 'compiled-valuable)))
      (let ((compiler::*compiler-warnings* nil))
        (setf (compiled-valuable p) (compile nil (getter p)))))
  (slot-value p 'compiled-valuable))

(defmethod is-numeric ((p property))
  "Answer whether <p> is a numeric property."
  (or (eq (data-type p) 'integer)
      (eq (data-type p) 'number)))


;; #NOTE: Not used yet
(defmethod possible-property-data-types ()
  "Answers the possible data types for properties."
  '(boolean
    number
    integer
    symbol
    string
    list
    list-structure
    base-model
    object))

;; #TODO: Try to avoid backup references of subject and update-callback
(defmethod copy ((p property))
  "NOTE: Redefined to avoid circular-graph recursion."
  (let ((subject-backup (subject p))
        (update-callback-backup (update-callback p)))
    (setf (subject p) nil
          (update-callback p) nil)
    (let ((c (copy-instance p))) 
      (setf (subject p) subject-backup
            (subject c) subject-backup
            (update-callback p) update-callback-backup
            (update-callback c) update-callback-backup)
      c)))

(defun get-property-chain-value (object chain)
  ;; Go to last property of the chain
  (let ((final-object object))
    (dolist (j chain)
      (if (eql j (car (last chain)))
          ;; Get value
          (return-from get-property-chain-value (get-chained-property-value final-object j))
        ;; Move to next object
        (setf final-object (get-value-for-property-named final-object j))))))

(defmethod get-chained-property-value (object name)
  (if (symbolp name)
      (get-value-for-property-named object name)
    (if (functionp name)
        (apply name (list object))
      (error "Bad property"))))

(defmethod valid-value-p (object property)
  (let ((value (handler-case (get-value-for-property object property) (error (function) nil))))
    (valid-dependency-value (dependency property) property value)))

(defmethod set-default-dependent-property-values ((o base-model))
  "Set default values for <o> dependant properties of <o> properties."
  (set-default-dependent-property-values-recursive o o (properties o)))

(defmethod set-default-dependent-property-values-recursive ((o base-model) (target base-model) properties)
  "Set default values for <o> dependant properties of <o> properties."
  ;; Update <o> dependant properties
  (dolist (i properties)
    ;; #TODO: Move up one level in iterations and it'll be faster
    (dolist (j (all-dependent-properties target))
      (when (has-dependency (first j) i)
        (set-default-dependent-property (second j) (first j) (get-value-for-property o i)))))
  ;; Continue updating tree if necessary
  (dolist (j (dependency-list target))
    (let ((value (handler-case (get-value-for-property o j) (error (function) nil))))
      (when value 
        (set-default-dependent-property-values-recursive o value (properties o))))))

(defun set-default-dependent-property (target property dependent-value)
  "Set default values for <object> properties which depend on <property>."
  (setf (possible-values property) (funcall (possible-values-function property) dependent-value)
        (default-value property) (funcall (default-value-function property) dependent-value))
  (ensure-valid-value target property))

;; #TODO: Better in functional style
(defmethod all-dependent-properties (object)
  (let ((result (dependent-properties object)))
    (dolist (i (dependency-list object))
      (let ((value (handler-case (get-value-for-property object i) (error (function) nil))))
        (appendf result (dependent-properties value))))
    result))

(defmethod non-dependent-properties (object)
  (reject (properties-definition object) (lambda (object) (dependency object))))

(defmethod dependent-properties (object)
  (if object
      (mapcar
       (lambda (o) (list o object))       
       (select (properties-definition object)
               (lambda (o) (dependency o))))))

(defmethod dependency-list ((o t))
  (select (properties o) (lambda (object) (equal (data-type object) 'model))))

(defmethod ensure-valid-value (object property)
  (when (not (valid-value-p object property))
    (set-value-for-property object property (default-value property))))

(defmethod ensure-valid-model-properties (object)
  (dolist (p (dependency-list object))
    (when (not (valid-value-p object p))
      (set-value-for-property object p (default-value p)))))