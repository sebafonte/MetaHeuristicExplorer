
(defclass selection-method (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (description :initarg :description :accessor description)))


(defmethod initialize-properties :after ((o selection-method))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'text-editor)
   (:name 'filter-function :label "Filter function" :accessor-type 'property-accessor-type 
    :data-type 'symbol :default-value nil :editor 'text-editor)))

(defmethod print-object ((o selection-method) seq)
  (format seq "~A" (description o)))

; -----------------------------------------------------------------------------

(defclass random-selection (selection-method) ())


(defmethod perform-selection ((o random-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result))
    (dotimes (i number)
      (let ((a (aref (individuals-array population) (random-integer 0 (size population)))))
        (setf result (nconc result (list a)))))
    result))

(defclass tournament-selection (selection-method) ())


(defmethod perform-selection ((o tournament-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result)
        (size (size population)))
    (dotimes (i number)
      (let ((a (aref (individuals-array population) (random-integer 0 size))) 
            (b (aref (individuals-array population) (random-integer 0 size))))
        (setf result (nconc result (list (best-of a b))))))
    result))

; -----------------------------------------------------------------------------

(defclass n-tournament-selection (selection-method) 
  ((tournament-size :initarg :tournament-size :accessor tournament-size)))


(defmethod initialize-properties :after ((object n-tournament-selection))
  "Initialize <o> properties."
  (add-properties-from-values
   object 
   (:name 'tournament-size :label "Tournament size" :accessor-type 'property-accessor-type 
    :data-type 'symbol :default-value 2 :editor 'text-editor)))

(defmethod perform-selection ((o n-tournament-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result)
        (size (size population)))
    (dotimes (i number)
      (let ((tournament))
        (dotimes (j (tournament-size o))
          (appendf tournament (aref (individuals-array population) (random-integer 0 size))))
        (appendf result (list (reduce 'better-than (to-list array))))))
    result))

; -----------------------------------------------------------------------------

(defclass ranking-proportionate-selection (selection-method) ())


(defmethod perform-selection ((o ranking-proportionate-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (normalize-and-sort-population-fitness population)
  (mapcar (lambda (i) (get-individual population i)) 
          (select-ranking-proportionate-index population number)))
 
(defmethod select-ranking-proportionate-index ((p population) n)
  "Answer <n> individuals from <p> using rank based selection.
   #NOTE: Population fitness need to be normalized."
  (let ((result)
        (sum-ranking 0)
        (size (size p)))
    (dotimes (i (1+ size))
      (incf sum-ranking i))
    (dotimes (c n)
      (let ((sum 0.0) 
            (value (random-real 0 sum-ranking)))
        (appendf result 
                 (list (do ((i 0 (1+ i)))
                           ((or (>= i size) (>= (+ sum (1+ i)) value))
                            (crop 0 (1- size) i))
                         (incf sum (1+ i)))))))
    result))

; -----------------------------------------------------------------------------

(defclass ranking-inverse-proportionate-selection (selection-method) ())


(defmethod perform-selection ((o ranking-inverse-proportionate-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (normalize-and-sort-population-fitness-inverse population)
  (mapcar (lambda (i) (get-individual population i)) 
          (select-ranking-proportionate-index population number)))

; -----------------------------------------------------------------------------

(defclass fitness-proportionate-selection (selection-method) ())


(defmethod perform-selection ((o fitness-proportionate-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (normalize-population-fitness population #'fitness)
  (mapcar (lambda (i) (get-individual population i)) 
          (select-fitness-proportionate-index population number)))

(defmethod select-fitness-proportionate-index ((p population) n)
  "Answer <n> individuals from <p> fitness proportionate based selection."
  (let ((result))
    (dotimes (c n)
      (let ((sum 1.0) 
            (size (size p)) 
            (value (random-real 0 1)))
        (setf result 
              (nconc result 
                     (list (do ((i 0 (1+ i)))
                               ((or (>= i size) (<= (- sum (fitness-normalized (get-individual p i)))
                                                    value))
                                (crop 0 (1- size) i))
                             (decf sum (fitness-normalized (get-individual p i)))))))))
    result))

; -----------------------------------------------------------------------------

(defclass fitness-inverse-proportionate-selection (selection-method) ())


;;; #NOTE: By the moment, this method dont need to normalize population fitness, it's done within
(defmethod perform-selection ((o fitness-inverse-proportionate-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (mapcar (lambda (i) (get-individual population i))
          (select-fitness-inverse-proportionate-index population number)))

(defmethod select-fitness-inverse-proportionate-index ((p population) n)
  "Answer <n> individuals from <p> inverse fitness proportionate based selection."
  (let ((result))
    (dotimes (c n)
      (let* ((sum 0.0) 
             (value (random-real 0 1))
             (registry (make-hash-table))
             (individuals (individuals p))
             (size (length individuals))
             (max-fitness (fitness (if individuals (first individuals)))))
        ;; Detect maximum fitness value
        (dolist (i individuals)
          (setf max-fitness (max (fitness i) max-fitness)))
        ;; Invert fitness of each individual and register it
        (dolist (i individuals)
          (let ((new-fitness (/- max-fitness (fitness i))))
            (setf (gethash i registry) new-fitness)
            (incf sum new-fitness)))
        ;; Re normalize values in hash table
        (dolist (i individuals)
          (setf (gethash i registry) (/ (gethash i registry) sum)))
        ;; Count and select individual (roulette wheel)
        (setf sum 0.0)
        (appendf result
                 (list (do ((i 0 (1+ i)))
                           ((or (>= i size) (>= sum value))
                            (crop 0 (1- size) i))
                         (incf sum (gethash (nth i individuals) registry)))))))
    result))

; -----------------------------------------------------------------------------

(defclass best-fitness-selection (selection-method) ())


(defmethod perform-selection ((o best-fitness-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result))
    (dotimes (c number)
      (appendf result (list (index-best-individual population))))
    (mapcar (lambda (i) (get-individual population i))
            result)))

; -----------------------------------------------------------------------------

(defclass worst-fitness-selection (selection-method) ())


(defmethod perform-selection ((o worst-fitness-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result))
    (dotimes (i number)
      (appendf result (list (index-worst-individual population))))
    (mapcar (lambda (i) (get-individual population i))
            result)))

#|
(defclass best-not-repeated-fitness-selection (selection-method) ())


(defmethod perform-selection ((o best-not-repeated-fitness-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result))
    (dotimes (c number)
      (appendf result (list (index-best-individual population))))
    (mapcar (lambda (i) (get-individual population i))
            result)))

; -----------------------------------------------------------------------------

(defclass worst-not-repeated-fitness-selection (selection-method) ())


(defmethod perform-selection ((o worst-not-repeated-fitness-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (let ((result))
    (dotimes (i number)
      (appendf result (list (index-worst-individual population))))
    (mapcar (lambda (i) (get-individual population i))
            result)))

; -----------------------------------------------------------------------------

(defclass fitness-proportionate-weighted-selection (selection-method) ())


(defmethod initialize-properties :after ((o fitness-proportionate-weighted-selection))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'weight-function :label "Weight function" :accessor-type 'property-accessor-type 
    :data-type 'symbol :default-value nil :editor 'text-editor)))

(defmethod perform-selection ((o fitness-proportionate-weighted-selection) population number)
  (seleccionar-fitness-proportionate population number))

; -----------------------------------------------------------------------------

(defclass random-weighted-selection (selection-method) ())


(defmethod initialize-properties :after ((o random-weighted-selection))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'weight-function :label "Weight function" :accessor-type 'property-accessor-type 
    :data-type 'symbol :default-value nil :editor 'text-editor)))

(defmethod perform-selection ((o random-weighted-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (seleccionar-random population number))

; -----------------------------------------------------------------------------

(defclass ranking-proportionate-weighted-selection (selection-method) ())


(defmethod initialize-properties :after ((o ranking-proportionate-weighted-selection))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'weight-function :label "Weight function" :accessor-type 'property-accessor-type 
    :data-type 'symbol :default-value nil :editor 'text-editor)))

(defmethod perform-selection ((o ranking-proportionate-weighted-selection) population number)
  "Answer a list with <number> elements from <population> using <o>."
  (seleccionar-ranking population number))
|#


(defun initialize-selection-methods ()
  (system-add 
   (make-instance 'random-selection 
                  :name 'random-selection-method
                  :description "Random")
   (make-instance 'tournament-selection 
                  :name 'tournament-selection-method
                  :description "Tournament")
   (make-instance 'best-fitness-selection
                  :name 'best-fitness-selection-method
                  :description "Best fitness")
   (make-instance 'worst-fitness-selection
                  :name 'worst-fitness-selection-method
                  :description "Worst fitness")
   (make-instance 'ranking-proportionate-selection 
                  :name 'ranking-selection-method
                  :description "Ranking proportional")
   (make-instance 'ranking-inverse-proportionate-selection
                  :name 'ranking-inverse-selection-method
                  :description "Ranking inverse")
   (make-instance 'fitness-proportionate-selection 
                  :name 'fitness-proportionate-selection-method
                  :description "Fitness proportional")
   (make-instance 'fitness-inverse-proportionate-selection 
                  :name 'inverse-fitness-selection-method
                  :description "Fitness inverse")))

(defun system-selection-methods ()
  (list 
   (system-get 'random-selection-method)
   (system-get 'tournament-selection-method)
   (system-get 'ranking-selection-method)
   (system-get 'ranking-inverse-selection-method)
   (system-get 'fitness-proportionate-selection-method)
   (system-get 'inverse-fitness-selection-method)
   (system-get 'best-fitness-selection-method)
   (system-get 'worst-fitness-selection-method)))
