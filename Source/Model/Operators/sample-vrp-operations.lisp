
(defclass sample-vrp-crossover (crossover)
  ())

(defmethod initialize-properties :after ((object sample-vrp-crossover))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'value-function :label "Crossover function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'function-editor :possible-values (possible-vrp-crossovers object))))

(defmethod possible-vrp-crossovers ((object sample-vrp-crossover))
  '(crossover-ox-sample-vrp
    sample-vrp-subtour-crossover
    ;sample-vrp-comparation-crossover
    ))


(defclass sample-vrp-mutation (mutation)
  ())


(defmethod initialize-properties :after ((object sample-vrp-mutation))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'value-function :label "Mutation function" :accessor-type 'accessor-accessor-type 
    :data-type 'list :editor 'function-editor :possible-values (possible-vrp-mutations object))))

(defmethod possible-vrp-mutations ((object sample-vrp-mutation))
  '(sample-vrp-city-in-tour-mutate
    sample-vrp-city-inter-tour-mutate
   ;sample-vrp-tour-mutate
    sample-vrp-split-tour-mutate
    sample-vrp-merge-tour-mutate))


;;; CROSSOVERS TEST
(defun sample-vrp-subtour-crossover (object-a object-b language operator)
  "Answer an individual applying subtour crossover from <program-a> to <program-b>."
  (declare (ignore language operator))
  (let* ((subtour (select-random-vrp-subtour (program object-a)))
         (new-tour (append (program object-b) (list subtour))))
    (make-instance 'entity-sample-vrp 
                   :expresion (remove-cities-from-last-vrp-subtour new-tour))))

#|
;; #TODO: 
(defun sample-vrp-comparation-crossover (program-a program-b language operator)
  "Answer an individual product of a 'differential' crossover, which pretent to explot local
   optimization when doing a crossover."
  nil)
|#

;;; MUTATIONS TEST
(defun sample-vrp-city-in-tour-mutate (object language operator)
  "Answer a mutated individual in one city of the same tour."
  (declare (ignore language operator))
  (let* ((program (program object))
         (target-tour-index (random-integer 0 (length program)))
         (target-tour (nth target-tour-index program))
         (index-a (random-integer 0 (length target-tour)))
         (index-b (random-integer 0 (length target-tour)))
         (a (nth index-a target-tour))
         (b (nth index-b target-tour))
         (result-a)
         (result))
    (setf result-a (replace-city-in-tour-by program target-tour-index index-a b)
          result (replace-city-in-tour-by result-a target-tour-index index-b a))
    (make-instance 'entity-sample-vrp :expresion result)))

(defun sample-vrp-city-inter-tour-mutate (object language operator)
  "Answer a mutated individual in one city of different tours."
  (declare (ignore language operator))
  (let* ((program (program object))
         (target-tour-a-index (random-integer 0 (length program)))
         (target-tour-b-index (random-integer 0 (length program))))
    (if (= target-tour-a-index target-tour-b-index)
        (setf target-tour-b-index (mod (1+ target-tour-b-index) (length program))))
    (let* ((target-tour-a (nth target-tour-a-index program))
           (target-tour-b (nth target-tour-b-index program))
           (index-a (random-integer 0 (length target-tour-a)))
           (index-b (random-integer 0 (length target-tour-b)))
           (a (nth index-a target-tour-a))
           (b (nth index-b target-tour-b))
           (result-a)
           (result))
      (setf result-a (replace-city-in-tour-by program target-tour-a-index index-a b)
            result (replace-city-in-tour-by result-a target-tour-b-index index-b a))
      (make-instance 'entity-sample-vrp :expresion result))))

(defun sample-vrp-split-tour-mutate (object language operator)
  "Answer an individual with a tour splitted for a new vehicle."
  (declare (ignore language operator))
  (let* ((tour (program object))
         (tour-length (length tour))
         (selection-tour-list (selection-tour-list tour))
         (result-expresion tour))
    (if selection-tour-list
        (let* ((selection-tour-list-length (length selection-tour-list))
               (subtour-index (nth (random-integer 0 selection-tour-list-length) 
                                   selection-tour-list))
               (final-tour)
               (splitted-tour))
          (dotimes (i tour-length)
            (if (= i subtour-index)
                (setf splitted-tour (split-subtour (nth i tour)))
              (appendf final-tour (list (nth i tour)))))
          (setf result-expresion (if splitted-tour 
                                     (append final-tour splitted-tour)
                                   final-tour))))
    (make-instance 'entity-sample-vrp :expresion result-expresion)))

(defun selection-tour-list (plan)
  (let ((selection-list)
        (index 0))
    (dolist (tour plan)
      (if (> (length tour) 1)
          (appendf selection-list (list index)))
      (incf index))
    selection-list))

(defun split-subtour (subtour-list)
  (let ((split-index (random-integer 0 (length subtour-list))))
    (if (= split-index 0) (setf split-index 1))
    (list (subseq subtour-list 0 split-index)
          (subseq subtour-list split-index))))

(defun sample-vrp-merge-tour-mutate (object language operator)
  "Answer an individual with a merged tour and a vehicle less."
  (declare (ignore language operator))
  (let* ((tour (program object))
         (tour-length (length tour))
         (index-a (random-integer 0 tour-length))
         (index-b (random-integer 0 tour-length))
         (rest-tour)
         (merged-tour))
    (if (= index-a index-b) 
        (setf index-a (mod (1+ index-a) tour-length)))
    (dotimes (i (length tour))
      (if (or (= i index-a) (= i index-b))
          (appendf merged-tour (nth i tour))
        (appendf rest-tour (list (nth i tour)))))
    (make-instance 'entity-sample-vrp :expresion (append (list merged-tour) rest-tour))))

#|
(defun sample-vrp-tour-mutate (program language operator)
  "Answer some mutated individuals of a tour with necesary corrections."
  nil)
|#

(defun replace-city-by (object index element)
  (let ((counter 0)
        (result)
        (pre-result))
    (dolist (i object)
      (dolist (j i)
        (appendf pre-result
                 (list (if (= index (incf counter)) element j))))
      (appendf result (list pre-result))
      (setf pre-result nil))
    result))

(defun replace-city-in-tour-by (object tour-index index element)
  (let ((tour-counter 0)
        (counter 0)
        (result)
        (pre-result))
    (dolist (i object)
      (setf counter 0)
      (dolist (j i)
        (appendf pre-result
                 (list (if (and (= tour-counter tour-index)
                                (= index counter))
                           element j)))
        (incf counter))
      (appendf result (list pre-result))
      (setf pre-result nil)
      (incf tour-counter))
    result))

(defun visited-cities (object)
  (let ((counter 0))
    (dolist (i object)
      (dolist (j i)
        (incf counter)))
    counter))

#|
(defun has-repeated-cities (object)
  (let ((counter 0)
        (table (make-hash-table)))
    (dolist (i object)
      (dolist (j i)
        (if (gethash j table)
            (return-from has-repeated-cities t)
          (setf (gethash j table) t))))))
|#

(defun select-random-vrp-subtour (tour)
  "Answer a random subtour from <tour>."
  (nth (random-integer 0 (length tour)) tour))

(defun remove-cities-from-last-vrp-subtour (tour)
  "Remove repeated cities in <tour> starting from the end."
  (let* ((last-subtour (car (last tour)))
         (new-tour (remove last-subtour (copy tour) :test 'equal)))
    (remove nil
            (append
             (mapcar (lambda (subtour) (correct-vrp-subtour subtour last-subtour)) new-tour)
             (list last-subtour)))))

(defun correct-vrp-subtour (tour new-tour)
  "Correct <tour> using <new-tour>."
  (reject tour (lambda (city) (includes new-tour city))))

