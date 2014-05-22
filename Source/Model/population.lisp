(defparameter *default-population-size* 100)


(defclass population (base-model)
  ((individuals-array :initarg :individuals-array :initform nil :accessor individuals-array)
   (count-individuals :initarg :count-individuals :initform *default-population-size* :accessor count-individuals)))


(defmethod initialize-instance :after ((p population) &key count-individuals individuals-array)
  "Initialize <p>."
  (setf (slot-value p 'individuals-array)
        (if individuals-array 
            individuals-array
          (make-array count-individuals :initial-element nil))))

(defmethod ensure-population-size ((p population) size)
  "Adjust <p> to <size> cropping at the end if neccesary."
  (when (not (= size (count-individuals p)))
    (let ((array (individuals-array p)))
      (setf (individuals-array p) (make-array size :initial-element nil)
            (count-individuals p) size)
      (dotimes (i size)
        (when (< i (length array))
          (setf (aref (individuals-array p) i) (aref array i)))))))
    
(defmethod get-individual ((p population) i)
  "Answer the individual indexed by <i> in <p>."
  (aref (individuals-array p) i))

(defmethod individuals ((p population))
  "Answer a list with all <p> individuals."
  (to-list-without-nils (individuals-array p)))

(defmethod index-individual ((p population) individual)
  "Answer the index of <individual> in <p>."
  (dotimes (i (population-size p))
    (if (eql (aref (individuals-array p) i)
             individual)
        (return-from index-individual i)))
  nil)

(defmethod population-size ((p population))
  "Answer <p> max size."
  (count-individuals p))

(defmethod individuals-count ((p population))
  "Answer the actual amount of individuals in <p>."
  (list-length (individuals p)))

(defmethod (setf individuals) (individuals (p population))
  "Setter for <p> individuals."
  (setf (slot-value p 'count-individuals) (length individuals)
        (slot-value p 'individuals-array) individuals))

(defmethod normalize-population-fitness ((p population) fitness-function)
  "Fill normalized values for <p> using <fitness-function>."
  (let* ((individuals (individuals-array p))
         (length (length individuals))
         (sum 0.0))
    ;; Sumarize adjusted fitness
    (dotimes (i length)
      (let* ((individual (aref individuals i))
             (fitness-value (apply fitness-function (list individual))))
        (incf sum fitness-value)
        (setf (fitness-adjusted individual) 
              (/ 1.0 (+ 1.0 (apply fitness-function (list individual)))))))
    ;; Normalize population adjusted fitness
    (dotimes (i length)
      (let ((individual (aref individuals i)))
        (setf (fitness-normalized individual)
              (if (> sum 0.0) 
                  (/ (fitness individual) sum)
                ;; Else: 0 (or 0.0)
                (fitness individual)))))))

(defmethod sort-population-fitness ((p population))
  "Sort population by it's individual fitness."
  (sort (individuals-array p) #'> :key #'fitness-normalized))

(defmethod normalize-and-sort-population-fitness ((p population))
  "Answer <p> individuals with normalized fitness sorted ascending."
  (normalize-population-fitness p #'fitness)
  (sort (individuals-array p) #'> :key #'fitness-normalized))

(defmethod normalize-and-sort-population-fitness-inverse ((p population))
  "Answer <p> individuals with normalized fitness sorted descending."
  (normalize-population-fitness p #'fitness-inverse)
  (sort (individuals-array p) #'> :key #'fitness-normalized))

;; #TODO: Refactor
(defmethod best-individual ((p population))
  "Anwer the best individual found by <p>."
  (let ((best-individual (aref (individuals-array p) 0)))
    (do ((i 1 (1+ i)))
        ((>= i (count-individuals p)))
      (if (better-than (aref (individuals-array p) i) best-individual)
          (setf best-individual (aref (individuals-array p) i))))
    best-individual))

(defmethod best-individual ((list list))
  "Answer the best individual of <list>."
  (map-best list 'better-than))

(defun map-best (list function)
  (let ((best (first list)))
    (dolist (i (cdr list))
      (if (apply function (list i best))
          (setf best i)))
    best))
    
(defmethod worst-individual ((p population))
  "Answer the <p> worst individual."
  (let ((worst (aref (individuals-array p) 0)))
    (do ((i 1 (1+ i)))
        ((>= i (count-individuals p)))
      (if (not (better-than (aref (individuals-array p) i) worst))
          (setf worst (aref (individuals-array p) i))))
    worst))

(defmethod index-best-individual ((p population))
  "Answer the index of <p> best individual."
  (let ((best (aref (individuals-array p) 0))
        (index 0))
    (do ((i 1 (1+ i)))
        ((>= i (count-individuals p)))
      (if (better-than (aref (individuals-array p) i) best)
          (setf best (aref (individuals-array p) i)
                index i)))
    index))

(defmethod index-worst-individual ((p population))
  "Answer the index of the worst individual of <p>."
  (let ((worst (aref (individuals-array p) 0))
        (index 0))
    (do ((i 1 (1+ i)))
        ((>= i (count-individuals p)))
      (if (not (better-than (aref (individuals-array p) i) worst))
          (setf worst (aref (individuals-array p) i)
                index i)))
    index))

(defmethod medium-value ((p population) function)
  "Answer the <function> medium value for <p>."
  (let ((acum 0))
    (dotimes (i (count-individuals p))
      (incf acum (funcall function (aref (individuals-array p) i))))
    (/ acum (count-individuals p))))

(defmethod best-individuals ((p population) n)
  "Answer the <n> best individuals of <p>."
  (let* ((individuals (copy-seq (individuals-array p)))
         (sorted-individuals (sort individuals #'> :key #'fitness))
         (result))
    (dotimes (i (min (count-individuals p) n))
      (push (aref sorted-individuals i) result))
    result))

(defmethod sorted-property-values-map ((p population) (property symbol))
  "Answer a list with ordered property values for <p> individuals."
  (let* ((individuals (copy-seq (individuals-array p)))
         (sorted-individuals (sort individuals #'< :key property))
         (index 0)
         (result))
    (dolist (i (to-list-without-nils sorted-individuals))
      (push (list index (funcall property i)) result)
      (incf index))
    (nreverse result)))

(defmethod add-individual ((p population) o) 
  "Add <o> to <p> when possible."
  (let* ((individuals (individuals-array p))
         (position (position-if #'null individuals)))
    (setf (aref individuals position) o)))

(defmethod delete-individual ((p population) object)
  "Delete <o> ocurrences from <p>."
  (replace-individual p object nil)
  (compact-population p))

(defmethod clear-population ((p population))
  "Clear <p>."
  (dotimes (i (count-individuals p))
    (setf (aref (individuals-array p) i) nil)))

(defmethod replace-individual ((p population) object replacement) 
  "Replace <object> with <replacement> ocurrences in <p>."
  (nsubstitute replacement object (individuals-array p)))

(defmethod simplify-strategy ((p population) strategy language)
  "Simplify <p> individuals using <strategy>."
  (dotimes (i (count-individuals p))
    (individual-simplification (aref (individuals-array p) i) language)))

(defmethod subpopulation ((p population) start end)
  "Answer a new population with <p> individuals from <start> to <end>."
  (let* ((size (- end start))
         (new-population (make-instance 'population :count-individuals size)))
    (dotimes (i size)
      (setf (aref (individuals-array new-population) (+ start i))
            (aref (individuals-array p) (+ start i))))
    new-population))

(defmethod subpopulation ((p array) start end)
  "Answer a new population with <p> individuals from <start> to <end>."
  (let* ((size (- end start))
         (new-population (make-instance 'population :count-individuals size)))
    (dotimes (i size)
      (setf (aref (individuals-array new-population) (+ start i))
            (aref p (+ start i))))
    new-population))

(defmethod make-population-with (individuals)
  "Answer a new population for <individuals>."
  (make-instance 'population :individuals-array individuals :count-individuals (length individuals)))