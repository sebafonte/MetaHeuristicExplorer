
(defclass ephemeral-random-constants-factory (constant-factory)
  ((min-value :initarg :min-value :initform 0 :accessor min-value)
   (max-value :initarg :max-value :initform 5 :accessor max-value)
   (result-type :initarg :result-type :initform 'double :accessor result-type)))


(defmethod initialize-properties :after ((o ephemeral-random-constants-factory))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :editor 'one-line-lisp-editor :default-value "Ephemeral random 0-5d")
   (:name 'min-value :label "Min value" :accessor-type 'accessor-accessor-type :object-parameter t
    :data-type 'number :min-value -1000000 :max-value 1000000 :default-value 0.0 :editor 'number-editor)
   (:name 'max-value :label "Max value" :accessor-type 'accessor-accessor-type :object-parameter t
    :data-type 'number :min-value -1000000 :max-value 1000000 :default-value 5.0 :editor 'number-editor)
   (:name 'result-type :label "Type" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'double :possible-values '('integer 'float 'double)
    :editor 'list-editor)))

(defmethod create-constant ((o ephemeral-random-constants-factory))
  (create-constant-type o (result-type o)))

(defmethod create-constant-type ((o ephemeral-random-constants-factory) (type (eql 'integer)))
  (random-integer (min-value o) (max-value o)))

;; #TODO: Agregar conversion!
(defmethod create-constant-type ((o ephemeral-random-constants-factory) (type (eql 'float)))
  (random-real (min-value o) (max-value o)))

(defmethod create-constant-type ((o ephemeral-random-constants-factory) (type (eql 'double)))
  (random-real (min-value o) (max-value o)))

(defmethod node-constant-p ((o ephemeral-random-constants-factory) node)
  (numberp node))
