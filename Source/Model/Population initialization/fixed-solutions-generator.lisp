
(defclass fixed-solutions-generator (population-generator)
  ((expresion-list :initarg :expresion-list :accessor expresion-list)))


(defmethod initialize-properties :after ((object fixed-solutions-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'fixed-solutions-generator :editor 'symbol-editor)
   (:name 'expresion-list :label "Expression list" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'one-line-lisp-editor)))

(defmethod generate-population ((generator fixed-solutions-generator) 
                                (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (fixed-solutions (expresion-list (initialization-method algorithm)))
         (size (length fixed-solutions)))
    (dotimes (i population-size)
      (let ((object (make-instance (objetive-class algorithm) :expresion (nth (mod i size) fixed-solutions))))
        (setf (aref population i) object)))
    (evaluate algorithm population)
    (make-instance 'population :count-individuals population-size :individuals-array population)))