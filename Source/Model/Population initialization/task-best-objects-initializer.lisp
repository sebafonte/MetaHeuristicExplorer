
(defclass task-best-objects-initializer (population-generator)
  ((input-task :initarg :input-task :accessor input-task)
   (random-select :initarg :random-select :accessor random-select)
   (mutate-individuals :initarg :mutate-individuals :accessor mutate-individuals)))


(defmethod initialize-properties :after ((object task-best-objects-initializer))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'input-task :label "Input task" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'button-editor)
   (:name 'random-select :label "Random select" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor)
   (:name 'mutate-individuals :label "Mutate" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((object task-best-objects-initializer) (algorithm search-algorithm))
  "Generate population on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size))
         (individuals (best-individuals (input-task object) population-size)))
    (dotimes (i (population-size algorithm))
      (setf (aref population i) (random-element individuals)))
    (make-instance 'population :count-individuals population-size :individuals-array population)))