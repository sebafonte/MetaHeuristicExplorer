
(defclass task-random-objects-initializer (population-generator)
  ((input-task :initarg :input-task :accessor input-task)))


(defmethod initialize-properties :after ((o task-random-objects-initializer))
  "Initialize <object> properties."
  (add-properties-from-values
   o
   (:name 'input-task :label "Input task" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)))

(defmethod generate-population ((o task-random-objects-initializer) (a search-algorithm))
  "Generate population on <algorithm>."
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (individuals (individuals (input-task o))))
    (dotimes (i population-size)
      (setf (aref population i) (random-element individuals)))
    (make-instance 'population :individuals-array population)))