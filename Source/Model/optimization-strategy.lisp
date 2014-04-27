
(defclass optimization-strategy (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (subject :initarg :subject :initform nil :accessor subject)
   (optimization-target :initarg :optimization-target :accessor optimization-target)
   (optimization-method :initarg :optimization-method :accessor optimization-method)))


(defmethod initialize-properties :after ((a optimization-strategy))
  "Initialize <a> properties."
  (add-properties-from-values
   a 
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string  :editor 'text-editor)
   (:name 'subject :label "Subject" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor :visible nil)
   (:name 'optimization-target :label "Target" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value (system-get-copy 'optimization-target-best-1)
    :possible-values (copy-tree (optimization-targets)) :editor 'configurable-copy-list-editor)
   (:name 'optimization-method :label "Method" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value nil :editor 'list-editor :possible-values (optimization-methods))))

(defmethod copy ((p optimization-strategy))
  "NOTE: Redefined to avoid circular-graph recursion."
  (let ((subject-backup (subject p)))
    (setf (subject p) nil)
    (let ((c (copy-instance p))) 
      (setf (subject p) subject-backup)
      c)))

(defmethod check-optimization ((o optimization-strategy))
  t)

(defmethod execute-optimization ((o optimization-strategy))
  (execute-optimization-on 
   (optimization-method o) 
   (target (optimization-target o) o)))

(defmethod execute-optimization :before ((o optimization-strategy))
  (trigger (subject o) :local-optimization-start o))

(defmethod execute-optimization :after ((o optimization-strategy))
  (trigger (subject o) :local-optimization-end o))

(defmethod check-execute-optimization ((o optimization-strategy))
  (if (check-optimization o)
      (execute-optimization o)))


(defclass generational-stage-optimization-strategy (optimization-strategy)
  ((max-generations :initarg :max-generations :accessor max-generations)))


(defmethod initialize-properties :after ((a generational-stage-optimization-strategy))
  "Initialize the properties of <a>."
  (add-properties-from-values
   a 
   (:name 'max-generations :label "Max generations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 20 :editor 'number-editor)))

(defmethod check-optimization ((o generational-stage-optimization-strategy))
  (let ((max-generations (max-generations o)))
    (and
     max-generations
     (= 0 (mod (generation (subject o)) max-generations)))))
  

(defclass iterational-stage-optimization-strategy (optimization-strategy)
  ((max-iterations :initarg :max-iterations :accessor max-iterations)))


(defmethod initialize-properties :after ((a iterational-stage-optimization-strategy))
  "Initialize the properties of <a>."
  (add-properties-from-values
   a 
   (:name 'max-iterations :label "Max iterations" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 10000 :editor 'number-editor)))

(defmethod check-optimization ((o iterational-stage-optimization-strategy))
  (let ((max-iterations (max-iterations o)))
    (and 
     max-iterations
     (= 0 (mod (iteration (subject o))
               max-iterations)))))


(defclass conditional-optimization-strategy (optimization-strategy)
  ((optimization-condition :initarg :optimization-condition :accessor optimization-condition)))


(defmethod initialize-properties :after ((a conditional-optimization-strategy))
  "Initialize the properties of <a>."
  (add-properties-from-values
   a
   (:name 'optimization-condition :label "Condition" :accessor-type 'accessor-accessor-type 
    :data-type 'object :default-value nil :editor 'list-editor)))

(defmethod check-optimization ((o conditional-optimization-strategy))
  (apply (optimization-condition o) (list o)))
