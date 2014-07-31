
(defclass polynomial-sample-generator (population-generator)
  ((min-terms :initarg :min-terms :accessor min-terms)
   (max-terms :initarg :max-terms :accessor max-terms)
   (min-factor :initarg :min-factor :accessor min-factor)
   (max-factor :initarg :max-factor :accessor max-factor)
   (min-grade :initarg :min-grade :accessor min-grade)
   (max-grade :initarg :max-grade :accessor max-grade)))


(defmethod initialize-properties :after ((object polynomial-sample-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'min-terms :label "Min terms" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 1 :editor 'integer-editor)
   (:name 'max-terms :label "Max terms" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 3 :editor 'integer-editor)
   (:name 'min-factor :label "Min factor" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 0 :editor 'number-editor)
   (:name 'max-factor :label "Max factor" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 10 :editor 'number-editor)
   (:name 'min-grade :label "Min grade" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 1 :editor 'integer-editor)
   (:name 'max-grade :label "Max grade" :accessor-type 'accessor-accessor-type 
    :data-type 'number :default-value 4 :editor 'integer-editor)))

(defmethod generate-population ((object polynomial-sample-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((variables (variables (language algorithm)))
         (population-size (population-size algorithm))
         (population (make-array population-size)))
    (dotimes (i (population-size algorithm))
      (let* ((terms (random-integer (min-terms object) (max-terms object)))
             (new-exp '(+)))
        (dotimes (i terms)
          (let ((factor (random-integer (min-factor object) (max-factor object)))
                (grade (random-integer (min-grade object) (max-grade object)))
                (variable-list))
            (dotimes (j grade)
              (appendf variable-list (list (random-element variables))))
            (appendf new-exp (list (list '* factor (cons '* variable-list))))))
        (let ((object (make-instance (objetive-class algorithm) :expresion new-exp)))
          (setf (aref population i) object))))
    (let ((new-population (make-instance 'population :count-individuals population-size :individuals-array population)))
      (evaluate algorithm new-population)
      new-population)))