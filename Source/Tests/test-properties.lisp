(defclass test-properties (test-base-model) ())

(defclass object-with-properties-for-test (object-with-properties)
  ((test-property-a :accessor test-property-a)))

(defclass object-with-properties-for-test-subclass (object-with-properties-for-test)
  ())


;; Default objects methods
(defmethod default-object-with-property ((o test-properties))
  "Answer a object-with-properties with one property."
  (let ((object (make-instance 'object-with-properties-for-test)))
    (add-properties-from-values object
     (:name 'test-property-a :label "Test property a" :accessor-type 'accessor-accessor-type 
      :data-type 'list :editor 'text-editor :default-value "Default value")
     (:name 'test-property-b :label "Test property b" :accessor-type 'property-accessor-type 
      :data-type 'list :editor 'text-editor :default-value "Default value")
     (:name 'test-property-c :label "Test property c" :accessor-type 'valuable-accessor-type 
      :data-type 'list :editor 'text-editor :default-value "Default value" 
      :getter '(lambda (object) "valuable value")))
    object))

(defmethod default-object-with-property-subclass ((o test-properties))
  "Answer a object-with-properties with one property."
  (let ((object (default-object-with-property o)))
    (add-properties-from-values object
     (:name 'test-property-a :label "Test property a (label overriden)" :accessor-type 'property-accessor-type 
      :data-type 'list :editor 'text-editor :default-value "Default value a overriden")
     (:name 'test-property-b :label "Test property b (label overriden)" :accessor-type 'property-accessor-type 
      :data-type 'integer :editor 'integer-editor :default-value "Default value b overriden"))    
    object))

;; Test methods
(defmethod test-add-property ((o test-properties))
 "Verifies whether properties can be added correctly to an instance of object-with-properties."
 (let* ((object (default-object-with-property o))
        (properties-size (length (properties-definition object))))
   (add-properties-from-values 
    object
    (:name 'test-property :label "Test property" :accessor-type 'accessor-accessor-type 
     :data-type 'list :editor 'text-editor :default-value "Default value"))
   (check (= (+ 1 properties-size) (length (properties-definition object))))))
   
(defmethod test-clean-properties ((o test-properties))
 "Verifies whether properties can be deleted correctly to an instance of object-with-properties."
 (let ((object (default-object-with-property o)))
   (clean-properties-definitions object)
   (check (= (length (properties-definition object)) 0))))

(defmethod test-property-accessors ((o test-properties))
 "Verifies whether properties can be deleted correctly to an instance of object-with-properties."
 (let* ((object (default-object-with-property o))
        (property (property-named object 'test-property-a)))
 (check 
   (equal (name property) 'test-property-a)
   (equal (label property) "Test property a")
   (equal (accessor-type property) 'accessor-accessor-type)
   (equal (data-type property) 'list)
   (equal (editor property) 'text-editor)
   (equal (default-value property) "Default value"))))

(defmethod test-property-named ((o test-properties))
 "Verifies whether properties can be obtained from an object-with-properties correctly."
 (let* ((object (default-object-with-property o)))
   (check 
     (not (null (property-named object 'test-property-a)))
     (not (null (property-named object 'test-property-b))))))

(defmethod test-value-for-property-named ((o test-properties))
 "Verifies whether value-for-property-named accessor works properly."
 (let ((object (default-object-with-property o)))
   (set-value-for-property-named object 'test-property-a "Some other value accessor")
   (set-value-for-property-named object 'test-property-b "Some other value property")
   (check 
     (equal (get-value-for-property-named object 'test-property-a) "Some other value accessor")
     (equal (get-value-for-property-named object 'test-property-b) "Some other value property")
     (equal (get-value-for-property-named object 'test-property-c) "valuable value"))))

(defmethod test-properties-copy ((o test-properties))
  "Verifies whether default value is the most specific for an aspect."
  (let ((object (default-object-with-property o)))
    (labels ((another-test-property-1 (object) 
               (declare (ignore object))
               1))
      ;; Add properties to object
      (add-properties-from-values 
       object
       (:name 'test-property-a :label "Test accessor accessor type property" 
        :accessor-type 'accessor-accessor-type :data-type 'list :editor 'text-editor 
        :default-value "Some default value for test accessor accessor type property")
       (:name 'test-property-b :label "Test property accessor type property" 
        :accessor-type 'property-accessor-type :data-type 'list :editor 'text-editor 
        :default-value "Some default value for test property accessor type property"))
      ;; Set property values
      (set-value-for-property-named object 'test-property-a "a")
      (set-value-for-property-named object 'test-property-b "b")
      ;; Check if object's copy has the correct values of object (default values)
      (check 
        (equal (get-value-for-property-named (copy object) 'test-property-a) "a")
        (equal (get-value-for-property-named (copy object) 'test-property-b) "b")))))

(defmethod test-add-valuable-property ((o test-properties))
  "Verifies no error when creating a new object-with-properties, adding a property
   to it and verifying it's correct value."
  (let ((object (make-instance 'object-with-properties)))
    (add-properties-from-values
     object
     (:name 'test-property
      :accessor-type 'valuable-accessor-type 
      :getter '(lambda (x) 3)
      :data-type 'integer))
    (get-value-for-property-named object 'test-property)))


(defclass test-properties-test-object (object-with-properties)
  ((name :initform :name :accessor name)))

(defmethod initialize-properties :after ((a test-properties-test-object))
  "Initialize <a> properties."
  (add-properties-from-values
   a
   (:name 'name :label "Name-b" :accessor-type 'accessor-accessor-type
    :data-type 'integer :default-value "name" :editor 'number-editor)))


(defclass test-properties-test-object-subclass (test-properties-test-object)
  ((name :initform :name :accessor name)))

(defmethod initialize-properties :after ((a test-properties-test-object-subclass))
  "Initialize <a> properties."
  (add-properties-from-values
   a
   (:name 'name :label "override" :accessor-type 'accessor-accessor-type
    :data-type 'integer :default-value "name b" :editor 'integer-editor)))

(defmethod test-subclass-property-override ((o test-properties))
  "Verifies whether inheritance of properties behaves correctly."
  (let* ((object (make-instance 'test-properties-test-object-subclass))
         (property (property-named object 'name)))
    (check
      (equal (label property) "override")
      (equal (editor property) 'integer-editor)
      (equal (data-type property) 'integer)
      (equal (name object) "name b"))))

(defmethod test-marshalled-object-properties ((o test-properties))
  "Verifies whether default properties are ok after marshalling <o>."
  (let* ((object (make-instance 'test-properties-test-object))
         (source (new-source-description object))
         (new-object (eval source))
         (property (property-named new-object 'name)))
    (check property)))
