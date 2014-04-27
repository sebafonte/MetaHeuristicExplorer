
;; <source> slot is intended to be used for a task or a file where subroutines could be loaded from.
(defclass subroutine-transfer-strategy (object-with-properties)
  ((name :initarg :name :accessor name)
   (percentaje :initarg :percentaje :accessor percentaje)
   (scoring-strategy :initarg :scoring-strategy :accessor scoring-strategy)
   (source :initarg :source :accessor source)))


(defmethod initialize-properties :after ((o subroutine-transfer-strategy))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "Subroutine transfer strategy" :data-type 'symbol :editor 'text-editor)
   (:name 'scoring-strategy :label "Scoring strategy" :accessor-type 'accessor-accessor-type 
    :default-value nil :data-type 'symbol :editor 'text-editor 
    :possible-values (list (system-get 'default-subroutine-equal-scoring-strategy)
                           (system-get 'default-subroutine-fitness-proportional-scoring-strategy)
                           (system-get 'default-subroutine-frequency-proportional-scoring-strategy)))))


(defclass file-subroutine-transfer-strategy (subroutine-transfer-strategy)
  ())


(defmethod initialize-properties :after ((o file-subroutine-transfer-strategy))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "File transfer strategy" :data-type 'symbol :editor 'text-editor)
   (:name 'source :label "Source" :accessor-type 'accessor-accessor-type 
    :default-value nil :data-type 'string :editor 'text-editor)))

(defmethod transfered-subroutines ((o file-subroutine-transfer-strategy))
  "Answer a hash-table with transfered subroutines."
  (filter-by-scoring-and-percentaje o nil))


(defclass task-subroutine-transfer-strategy (subroutine-transfer-strategy)
  ())


(defmethod initialize-properties :after ((o task-subroutine-transfer-strategy))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :default-value "Task transfer strategy" :data-type 'symbol :editor 'text-editor)
   (:name 'source :label "Source" :accessor-type 'accessor-accessor-type 
    :default-value nil :data-type 'object :editor 'text-editor)))

(defmethod transfered-subroutines ((o task-subroutine-transfer-strategy))
  "Answer a hash-table with transfered subroutines."
  (filter-subroutines
   (copy (subroutines (language (parent-task o))))))

(defmethod filter-subroutines ((o task-subroutine-transfer-strategy) subroutines)
  "Apply <o> scoring and extract <o> percentaje individuals from list."
  (filter-by-percentaje o 
   (sort-by-score-ascending o subroutines)))

(defmethod sort-by-score-ascending ((o task-subroutine-transfer-strategy) subroutines)
  (sort-by-population-score subroutines (population (source o))))

(defun filter-by-percentaje (sorted-subroutines percentaje)
  (let ((cut-point (ceiling (* (/ percentaje 100) (length sorted-subroutines)))))
    (subseq sorted-subroutines cut-point)))

(defun sort-by-population-score (strategy subroutines population)
  (let* ((individuals (individuals population))
         (scored-population (score-individuals strategy subroutine o)))
     (sort scored-population (lambda (a b) (> (cadr a) (cadr b))))))
     



