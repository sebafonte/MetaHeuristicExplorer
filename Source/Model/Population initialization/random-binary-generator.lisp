
(defclass random-binary-generator (population-generator)
  ((repeat-control :initarg :repeat-control :accessor repeat-control)))


(defmethod initialize-properties :after ((object random-binary-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'repeat-control :label "Repeat control" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((p random-binary-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (if (repeat-control p)
      (generate-population-incremental-size-attempts p a)
    (generate-population-no-control p a)))

(defmethod generate-population-incremental-size-attempts ((p random-binary-generator) (a search-algorithm))
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (max-attempts (max-unique-iterations a))
         (attempts 0))
    (do ((i 0))
        ((>= i population-size))
      (let ((value (generate-individual-value p (language a))))
        (cond 
         ;; New and not into registry
         ((not (gethash value (registry a)))
          (setf (aref population i) (make-instance (objetive-class a) :expresion value))
          (evaluate a (aref population i))
          (setf (gethash value (registry a)) t
                attempts 0)
               (incf i))
         ;; New and into registry: max retries reached
         ((> attempts max-attempts))
         ;; Found, did not reach max attempts #LOG
         (t (incf attempts)))))
    ;; #TODO: this should be moved to algorithm calling function
    (clrhash (registry a))
    (make-instance 'population :individuals-array population)))

(defmethod generate-population-no-control ((p random-binary-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (let* ((population-size (population-size a))
         (population (make-array population-size)))
    ;; Populate with random programs with no repeat control
    (do ((i 0))
        ((>= i population-size))
      (setf (aref population i) (generate-individual p a))
      (incf i))
    ;; Answer population object
    (let ((new-population (make-instance 'population :individuals-array population)))
      (evaluate a new-population)
      new-population)))

(defmethod generate-individual-value ((generator random-binary-generator) language)
  "Answer a new generated program tree on <language> using <generator>."
  (create-new-random-valid language nil))

(defmethod generate-individual ((generator random-binary-generator) algorithm)
  "Answer a new generated object on <language> using <generator>."
  (make-instance (objetive-class algorithm) 
                 :expresion (generate-individual-value generator (language algorithm))))
