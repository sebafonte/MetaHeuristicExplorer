
(defclass random-trees-generator (population-generator)
  ((min-size :initarg :min-size :accessor min-size)
   (max-size :initarg :max-size :accessor max-size)
   (min-depth :initarg :min-depth :accessor min-depth)
   (max-depth :initarg :max-depth :accessor max-depth)
   (use-top :initarg :use-top :accessor use-top)
   (use-full :initarg :use-full :accessor use-full)
   (repeat-control :initarg :repeat-control :accessor repeat-control)))


(defmethod initialize-properties :after ((object random-trees-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'min-size :label "Min size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor
    ;:dependency (make-eql-language-dependence 'objective-class)
    :default-value-function (lambda (objective-class) (lambda (objective-class) (min-size (default-language (make-instance objective-class))))))
   (:name 'max-size :label "Max size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor
    ;:dependency (make-eql-language-dependence 'objective-class)
    :default-value-function (lambda (objective-class) (lambda (objective-class) (max-size (default-language (make-instance objective-class))))))
   (:name 'min-depth :label "Min depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 1 :editor 'number-editor
    ;:dependency (make-eql-language-dependence 'objective-class)
    :default-value-function (lambda (objective-class) (lambda (objective-class) (min-depth (default-language (make-instance objective-class))))))
   (:name 'max-depth :label "Max depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor 
    ;:dependency (make-eql-language-dependence 'objective-class)
    :default-value-function (lambda (objective-class) (lambda (objective-class) (max-depth (default-language (make-instance objective-class))))))
   (:name 'use-top :label "Use top" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor)
   (:name 'use-full :label "Use full" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)
   (:name 'repeat-control :label "Repeat control" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((p random-trees-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (if (repeat-control p)
      (generate-population-incremental-size-attempts p a)
    (generate-population-no-control p a)))

(defmethod generate-population-incremental-size-attempts ((p random-trees-generator) (a search-algorithm))
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (min-tree-depth 0)
         (max-attempts (max-unique-iterations a))
         (attempts 0))
    (do ((i 0))
        ((>= i population-size))
      (let ((program (generate-program-tree p (language a))))
        (cond 
         ;; New and not into registry
         ((not (gethash program (registry a)))
          (setf (aref population i) (make-objective program))
          (evaluate a (aref population i))
          (setf (gethash program (registry a)) t
                attempts 0)
          (incf i))
         ;; New and into registry: max retries reached
         ((> attempts max-attempts)
          (incf min-tree-depth))
         ;; Found, did not reach max attempts #LOG
         (t (incf attempts)))))
    ;; #TODO: this should be moved to algorith calling function
    (clrhash (registry a))
    (make-instance 'population :individuals-array population)))

(defmethod generate-population-no-control ((o random-trees-generator) (a search-algorithm))
  "Generate population for search on <algorithm>.
  #NOTE: Fill population with new random invididuals."
  (let* ((population-size (population-size a))
         (population (make-array population-size)))
    ;; Populate with random programs with no repeat control
    (do ((i 0))
        ((>= i population-size))
      (let ((program (generate-program-tree o (language a))))
        (setf (aref population i) (make-objective a program))
        (incf i)))
    ;; Answer population object
    (let ((new-population (make-instance 'population :individuals-array population)))
      (evaluate a new-population)
      new-population)))

(defmethod generate-program-tree ((o random-trees-generator) language)
  "Answer a new generated program tree on <language> using <generator>."
  (let ((max-size (max-size-new-individuals language))
        (max-depth (max-depth-new-individuals language)))
    (simplify language (create-expresion language max-size max-depth (use-top o) (use-full o))))) 

(defmethod generate-individual ((o random-trees-generator) algorithm)
  "Answer a new generated object on <language> using <generator>."
  (make-objective algorithm (generate-program-tree generator (language algorithm))))
