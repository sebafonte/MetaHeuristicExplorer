
(defclass entity-image-bw-seamless (entity-image-bw object-with-properties)
  ((start-position-x :initarg :start-position-x :initform 0 :accessor start-position-x)
   (start-position-y :initarg :start-position-y :initform 0 :accessor start-position-y)
   (height :initarg :height :initform 1 :accessor height)
   (width :initarg :height :initform 1 :accessor width)))


(defmethod initialize-properties :after ((o entity-image-bw-seamless))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'start-position-x :label "Start x" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 0 :editor 'text-editor)  
   (:name 'start-position-y :label "Start y" :accessor-type 'accessor-accessor-type
    :data-type 'number :default-value 0 :editor 'number-editor)
   (:name 'height :label "Heigth" :accessor-type 'accessor-accessor-type
    :data-type 'number :default-value 1 :editor 'number-editor)
   (:name 'width :label "Width" :accessor-type 'accessor-accessor-type
    :data-type 'number :default-value 1 :editor 'number-editor)))

(defmethod default-fitness-evaluators ((o entity-image-bw-seamless))
  "Answer the default classes that can evaluate <o> fitness."
  (system-get 'entity-function-imagen-seamless-evaluator))
