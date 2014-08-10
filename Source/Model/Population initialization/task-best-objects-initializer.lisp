
(defclass task-best-objects-initializer (population-generator)
  ((input-task :initarg :input-task :accessor input-task)))


(defmethod initialize-properties :after ((o task-best-objects-initializer))
  "Initialize <object> properties."
  (add-properties-from-values
   o
   (:name 'input-task :label "Input task" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)))

(defmethod generate-population ((o task-best-objects-initializer) (a search-algorithm))
  "Generate population on <algorithm>."
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (individuals (best-individuals (input-task o) population-size)))
    (dotimes (i population-size)
      (setf (aref population i) (random-element individuals)))
    (make-instance 'population :individuals-array population)))