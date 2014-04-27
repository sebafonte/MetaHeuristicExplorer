
(defclass subroutine-replacement-strategy (object-with-properties)
  ())


(defclass no-limit-subroutine-replacement-strategy (subroutine-replacement-strategy)
  ())


(defclass biased-call-subroutine-replacement-strategy (subroutine-replacement-strategy)
  ((max-subroutines :initarg :max-subroutines :accessor max-subroutines)))

(defmethod initialize-properties :after ((o biased-call-subroutine-replacement-strategy))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'max-subroutines :label "Max subroutines" :accessor-type 'accessor-accessor-type 
    :default-value 5 :data-type 'integer :editor 'integer-editor)))


(defclass subtree-subroutine-replacement-strategy (subroutine-replacement-strategy)
  ((max-subroutines :initarg :max-subroutines :accessor max-subroutines)))

(defmethod initialize-properties :after ((o subtree-subroutine-replacement-strategy))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'max-subroutines :label "Max subroutines" :accessor-type 'accessor-accessor-type 
    :default-value 5 :data-type 'integer :editor 'integer-editor)))


