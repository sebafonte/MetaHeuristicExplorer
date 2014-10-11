 
(defclass evolutive-algorithm-sample-generator (search-task-sample-generator)
  ())


(defmethod initialize-properties :after ((o evolutive-algorithm-sample-generator))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'sample-objects :label "Sample algorithms" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value (default-objects o) :editor 'lisp-editor)))

(defmethod default-objects ((o evolutive-algorithm-sample-generator))
  (list
   (sample-composite-algorithm-code-1)))
