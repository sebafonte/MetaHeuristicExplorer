;; Responsibility:
;;
;;  - Do object creation
;;  - Fill new population with objects
;;  - Evaluates individuals
;;  - Check elites when creating individuals

(defclass breeder (object-with-properties)
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   ;(breeding-method :initarg :breeding-method :accessor breeding-method)
   (new-population :initarg :new-population :accessor new-population)))


(defmethod print-object ((o breeder) seq)
  (format seq "~A" (description o)))

(defmethod initialize-properties :after ((o breeder))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "Breeder" :editor 'string-editor)
   (:name 'description :label "Description" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "Breeder" :editor 'string-editor)
   #|
   (:name 'breeding-method :label "Breeding method" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'create-distinct-individual :editor 'list-editor
    :possible-values '(create-individual create-distinct-individual))
    |#
   ))


(defclass new-population-breeder (breeder)
  ((min-copies :initarg :min-copies :initform 0 :accessor min-copies)
   (max-copies :initarg :max-copies :initform 0 :accessor max-copies)
   (new-population :initarg :new-population :initform nil :accessor new-population)))


(defmethod initialize-properties :after ((o new-population-breeder))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'min-copies :label "Min copies" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 0 :editor 'one-line-lisp-editor
    :category "Parameters" :object-parameter t)
   (:name 'max-copies :label "Max copies" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100000 :default-value 0 :editor 'one-line-lisp-editor
    :category "Parameters" :object-parameter t)))

#|
;; #TODO: Add check to support population size change
(defmethod breeding-auxiliary-population ((o new-population-breeder) size)
  (when (null (new-population o))
    (setf (new-population o) (make-array size)))
  (new-population o))
|#

(defmethod breeding-auxiliary-population ((o new-population-breeder) size)
  (make-array size))

(defmethod breed-population ((a new-population-breeder) algorithm)
  (let* ((repeat-control (repeat-control algorithm))
         (population (population algorithm))
         (new-population (breeding-auxiliary-population a (population-size population)))
         (number-copies (random-integer (min-copies a) (max-copies a)))
         (copies (perform-selection (selection-method algorithm) population number-copies))
         (count-create (- (population-size population) number-copies))
         (start number-copies))
    ;; Copy and register
    (dotimes (i (length copies))
      (setf (aref new-population i) (nth i copies))
      (when repeat-control
        (register-individual algorithm (nth i copies))))
    ;; Create
    (dotimes (i count-create)
      (setf (aref new-population (+ i start)) 
            (if repeat-control 
                (create-distinct-individual algorithm nil)
              (create-individual algorithm nil))))
    ;; Evaluate
    (evaluate algorithm (subpopulation new-population start (+ start count-create)))
    ;; Reinsert population elites and normalize/order by fitness
    (setf (individuals-array population) new-population)))

(defclass existing-population-breeder (breeder)
  ((breed-count :initarg :breed-count :accessor breed-count)))


(defmethod initialize-properties :after ((o existing-population-breeder))
  "Initialize <object> properties."
  (add-properties-from-values
   o
   (:name 'breed-count :label "Breed count" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value 1 :editor 'integer-editor)))

(defmethod breed-population ((a existing-population-breeder) algorithm)
  (let ((child (if (repeat-control algorithm)
                    (create-distinct-individual algorithm)
                  (create-individual algorithm))))
    (check-elite elite-manager child)
    (replace-child algorithm child)))

;; Individual creation functions
(defmethod create-individual ((a evolutionary-algorithm) &optional (evaluate t))
  "Answer a new individual for <a>."
  (let* ((child (make-instance (objetive-class a)))
         (operation (select-genetic-operation a))
         (parents (perform-selection (selection-method a) (population a) (arity operation)))
         (programs (mapcar (lambda (i) (program i)) parents)))
    (create-child child operation a programs)
    (when evaluate (evaluate a child))
    child))

(defmethod create-distinct-individual ((a evolutionary-algorithm) &optional (evaluate t))
  "Answer a new generated children for <a> trying again until max iterations reached."
  (let* ((child (make-instance (objetive-class a)))
         (operation (select-genetic-operation a))
         (parents (perform-selection (selection-method a) (population a) (arity operation)))
         (max-iterations (max-unique-iterations a))
         (registry (registry a))
         (created))
    ;; Iterate to get an unique child
    (do ((i 0 (1+ i)))
        ((or created (>= i max-iterations)))
      (create-child child a operation parents)
      (setf created (not (gethash (program child) registry))))
    ;; #LOG: Random creation
    (when (not created) 
      (create-valid-child child a parents))
    ;; Register child in hash table
    (setf (gethash (program child) registry) t)
    child))
