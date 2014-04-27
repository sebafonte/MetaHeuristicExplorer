(defclass abstract-subroutine-manager (object-with-properties)
  ((name :initarg :name :accessor name)))

(defmethod abstractp ((o (eql 'abstract-subroutine-manager)))
 "Answer whether <o> is abstract."
 (eql o 'abstract-subroutine-manager))


(defclass language-subroutine-manager (abstract-subroutine-manager)
  ((language :initarg :language :accessor language)))


(defmethod initialize-properties :after ((o language-subroutine-manager))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "Subroutine manager" :data-type 'symbol :editor 'text-editor)))


(defclass null-subroutine-manager (abstract-subroutine-manager)
  ())


(defmethod initialize-properties :after ((o null-subroutine-manager))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "Null subroutine manager" :data-type 'symbol :editor 'text-editor)))
