
(defclass fixed-array-language (language)
  ((elements :initarg :elements :accessor elements)
   (possible-genes :initarg :possible-genes :accessor possible-genes)))


(defmethod initialize-properties :after ((o fixed-array-language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'elements :label "Elements" :accessor-type 'accessor-accessor-type :default-value 32 :data-type 'integer 
    :editor 'integer-editor)
   (:name 'possible-genes :label "Genes" :accessor-type 'accessor-accessor-type :default-value '(0 1 2 3 4) :data-type 'list 
    :editor 'list-editor)))

(defmethod mutate-value ((o fixed-array-language) value)
  (random-element
   (remove value (possible-genes o))))

(defmethod random-language-element ((l fixed-array-language))
  (random-element (possible-genes l)))

(defmethod create-new-random-valid ((l fixed-array-language) parents)
  (declare (ignore parents))
  (let ((value (make-array (elements l))))
    (dotimes (i (elements l))
      (setf (aref value i) (random-language-element l)))
    value))


(defclass binary-language (fixed-array-language)
  ())


(defmethod mutate-value ((o binary-language) value)
  (declare (ignore o))
  (if (= 0 value) 1 0))

(defmethod initialize-properties :after ((o binary-language))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'elements :label "Bits" :accessor-type 'accessor-accessor-type :default-value 32 :data-type 'integer 
    :editor 'integer-editor)
   (:name 'possible-genes :label "Genes" :accessor-type 'accessor-accessor-type :default-value '(0 1) :data-type 'list 
    :editor 'list-editor :read-only t)))

(defmethod create-new-random-valid ((l binary-language) parents)
  (declare (ignore parents))
  (let ((value (make-array (elements l) :element-type 'bit)))
    (dotimes (i (elements l))
      (setf (aref value i) (random-bit)))
    value))
