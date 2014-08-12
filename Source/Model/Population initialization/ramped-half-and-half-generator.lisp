
(defclass ramped-half-and-half-generator (population-generator)
  ((max-depth :initarg :max-depth :accessor max-depth)
   (use-top :initarg :use-top :accessor use-top)))


(defmethod initialize-properties :after ((object ramped-half-and-half-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'ramped-half-and-half-generator :editor 'symbol-editor)
   (:name 'max-depth :label "Max depth" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 1 :max-value 10000 :default-value 5 :editor 'number-editor)
   (:name 'use-top :label "Use top" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value t :editor 'boolean-editor)))

(defmethod generate-population ((o ramped-half-and-half-generator) (a search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((population-size (population-size a))
         (population (make-array population-size)))
    (dotimes (i population-size)
      (let ((tree (if (< (random-real 0 1) 0.5)
                      (generate-individual-grow o a)
                    (generate-individual-full o a))))
        (setf (aref population i) (make-objective a tree))))
    (let ((new-population (make-instance 'population :individuals-array population)))
      (evaluate a new-population)
      new-population)))

;; These functions has size check which differs from original Koza implementation:
;;  - If size wants to be avoided for shape reasons, needs to be setted with a high value
;;  - Top option to avoid creating single terminal trees

(defmethod generate-individual-grow ((p ramped-half-and-half-generator) a)
  (let ((language (language a)))
    (create-expresion language (max-size language) (max-depth p) t nil)))

(defmethod generate-individual-full ((p ramped-half-and-half-generator) a)
  (let ((language (language a)))
    (create-expresion language (max-size language) (max-depth p) t t)))