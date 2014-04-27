
(defclass entity-linear-ordering-list (entity-linear-ordering)
  ((permutations-list :initarg :permutations-list :initform nil :accessor permutations-list)
   (initial-matrix :initarg :initial-matrix :initform nil :accessor initial-matrix)))


(defmethod copy ((o entity-linear-ordering-list))
  (let ((new-instance (make-instance 'entity-linear-ordering-list
                                     :matrix (copy (matrix o))
                                     :permutations-list (copy (permutations-list o))
                                     :initial-matrix (initial-matrix o))))
    (setf (gen new-instance) (copy (gen o)))
    new-instance))

(defmethod calculate-matrix ((o entity-linear-ordering-list))
  "Calculate the final matrix for <o>."
  (let ((matrix-size (matrix-size o)))
    (setf (matrix o) (copy (initial-matrix o)))
    (dolist (i (permutations-list o))
      (funcall (first i) (matrix o) matrix-size (second i) (third i)))))

(defmethod calculate-matrix-from-last-value ((o entity-linear-ordering-list))
  (let ((i (car (last (permutations-list o)))))
    (funcall (first i) (matrix o) (matrix-size o) (second i) (third i))))

(defmethod permutate-random-row ((o entity-linear-ordering-list) algorithm operator)
  "Answer a new instance of <o> with a row permutated matrix."
  (permutate-with o #'permutate-row))
     
(defmethod permutate-random-column ((o entity-linear-ordering-list) algorithm operator)
  "Answer a new instance of o with a column permutated matrix."
  (permutate-with o #'permutate-column))

(defmethod delete-random-permutation ((o entity-linear-ordering-list) algorithm operator)
  (let* ((new-object (copy o))
         (best new-object))
    (dotimes (i 10)
      (let ((position))
        (setf (permutations-list new-object) ())
        (calculate-matrix new-object)
        (eval new-object)
        (if (> (fitness new-object) (fitness best))
            (setf best new-object))))
    best))

(defmethod permutate-with ((o entity-linear-ordering-list) function)
  (let ((new-object (copy o)))
    (appendf (permutations-list new-object) 
             (list (list function 
                         (random-integer 0 (matrix-size o))
                         (random-integer 0 (matrix-size o)))))
    (calculate-matrix-from-last-value new-object)
    new-object))

(defmethod permutate-best-row ((o entity-linear-ordering-list) algorithm operator)
  (permutate-best o algorithm #'permutate-row))

(defmethod permutate-best-column ((o entity-linear-ordering-list) algorithm operator)
  (permutate-best o algorithm #'permutate-column))

(defmethod permutate-best ((o entity-linear-ordering-list) algorithm function)
  (let* ((new-object (copy o))
         (source-index (random-integer 0 (matrix-size o)))
         (best-individual new-object)
         (matrix-size (matrix-size o)))
    (dotimes (i (matrix-size o))
      (setf new-object (copy o))
      (funcall function (matrix new-object) matrix-size i source-index)
      (evaluate algorithm new-object)
      (if (> (fitness new-object) (fitness best-individual))
          (setf best-individual new-object)))
    (if (<= (fitness o) (fitness best-individual))
        (permutate-with o function)
      best-individual)))

(defmethod permutate-next-row ((o entity-linear-ordering-list) algorithm operator)
  (permutate-next o algorithm #'permutate-row))

(defmethod permutate-next-column ((o entity-linear-ordering-list) algorithm operator)
  (permutate-next o algorithm #'permutate-column))

(defmethod permutate-next ((o entity-linear-ordering-list) algorithm function)
  (let ((new-object (copy o))
        (source-index (random-integer 0 (matrix-size o)))
        (matrix-size (matrix-size o))
        (best-individual))
    (do ((i 0 (1+ i)))
        ((or best-individual (>= i matrix-size)))
      (setf new-object (copy o))
      (funcall function (matrix new-object) matrix-size i source-index)
      (evaluate algorithm new-object)
      (if (> (fitness new-object) (fitness o))
          (setf best-individual new-object)))
    (if best-individual
        best-individual
      (permutate-with o function))))

(defmethod lop-crossover-3 ((object-a entity-linear-ordering-list) 
                            (object-b entity-linear-ordering-list) 
                            algorithm
                            operator)  
  "Answer a new individual with one-point crossover from <object-a>, <object-b> and a weight function."
  ;; #NOTE: Use with-slots with permutations-*
  (let* ((new-object-a (copy object-a))
         (new-object-b (copy object-b))
         (permutations-a (permutations-list new-object-a))
         (permutations-b (permutations-list new-object-b))
         (index-a (random-integer 0 (length permutations-a)))
         (index-b (random-integer 0 (length permutations-b))))
    (setf (permutations-list new-object-a)
          (append (subseq permutations-a 0 index-a)
                  (subseq permutations-b index-b))
          (permutations-list new-object-b)
          (append (subseq permutations-b 0 index-b)
                  (subseq permutations-a index-a)))
    (calculate-matrix new-object-a)
    (calculate-matrix new-object-b)
    (values new-object-a new-object-b)))

(defmethod lop-crossover-2 ((object-a entity-linear-ordering-list) 
                            (object-b entity-linear-ordering-list) 
                            algorithm
                            operator)
  (values (directional-crossover-2 object-a object-b algorithm)
          (directional-crossover-2 object-b object-a algorithm)))

(defmethod directional-crossover-2 ((object-a entity-linear-ordering-list) 
                                    (object-b entity-linear-ordering-list) 
                                    algorithm)  
  "Answer a new individual with one-point crossover of <object-a> and <object-b>."
  ;; #NOTE: Use with-slots with permutations-*
  (let* ((new-object (copy object-a))
         (permutations-a (permutations-list (copy object-a)))
         (permutations-b (permutations-list (copy object-b)))
         (index-a (random-integer 0 (length permutations-a)))
         (index-b (random-integer 0 (max (- (length permutations-b) index-a) 0))))
    (setf (permutations-list new-object) 
          (append (subseq permutations-b 0 (max (- (length permutations-b) 
                                                   (- (length permutations-a) index-a))
                                                0))
                  (subseq permutations-a index-a)))
    (calculate-matrix new-object)
    new-object))

(defmethod lop-crossover-1 ((object-a entity-linear-ordering-list) 
                            (object-b entity-linear-ordering-list)
                            algorithm
                            operator)
  (values (directional-crossover-3 object-a object-b algorithm)
          (directional-crossover-3 object-b object-a algorithm)))

(defmethod directional-crossover-3 ((object-a entity-linear-ordering-list) 
                                    (object-b entity-linear-ordering-list) 
                                    algorithm)  
  "Answer a new individual with one-point crossover of <object-a> and <object-b>."
  (let* ((new-object (copy object-b))
         (permutations-source (permutations-list (copy object-a)))
         (permutations-target (permutations-list (copy object-b)))
         (copy-length 2)
         (length-source (- (length permutations-source) copy-length))
         (length-target (- (length permutations-target) copy-length))
         (index-start-source (random-integer 0 length-source))
         (index-start-target (random-integer 0 length-target))
         (index-end-source (+ index-start-source copy-length))
         (index-end-target (+ index-start-target copy-length)))
  (setf (subseq (permutations-list new-object) index-start-target index-end-target)
        (subseq permutations-source index-start-source index-end-source))
  (calculate-matrix new-object)
  new-object))

(defmethod structure-size ((o entity-linear-ordering-list))
  "Answer the gene structure size of <o>."
  (length (permutations-list o)))

(defmethod simplification-1 ((o entity-linear-ordering-list) algorithm)
  "Answer a new individual with a simplified permutations list."
  (let ((best (copy o)))
    ;; Delete equal results
    (dolist (i (permutations-list best))
      (if (not (= (second i) (third i)))
          (appendf new-list (list i))))
    ;; #TODO: #DEBUG: Check result is always the same
    (evaluate new-object)
    (dotimes (i 10)
      ;; Delete while it's not worse
      (setf (permutations-list best) ())
      (calculate-matrix new-object)
      (evaluate new-object)
      (if (>= (fitness new-object) (fitness best))
          (setf best new-object
                old-fitness (fitness new-object))))))

(defmethod simplification-1 ((o entity-linear-ordering-list) algorithm)
  "Answer a new individual created with a simplification operation."
  (individual-simplification o algorithm (structure-size o)))

(defmethod individual-simplification ((o entity-linear-ordering-list) algorithm max-iterations)
  "Answer a new simplyfied individual."
  (let ((new-object (copy o))
        (previous))
    (do ()
        ((and new-object previous (= (structure-size previous) (structure-size new-object))))
      (setf previous new-object
            new-object (simplification-iteration new-object algorithm max-iterations)))
    new-object))

(defmethod simplification-iteration ((o entity-linear-ordering-list) algorithm max-iterations)
  "Answer a new individual with a simplified permutations list."
  (let* ((new-object)
         (best-individual (copy o)))
    (do ((i 0 (1+ i)))
        ((>= i (min (structure-size best-individual) max-iterations)))
      (setf new-object (copy best-individual))
      (let ((list (permutations-list new-object)))
        (setf (permutations-list new-object)
              (append (subseq list 0 i)
                      (subseq list (1+ i) (structure-size new-object)))))
      (calculate-matrix new-object)
      (evaluate algorithm new-object)
      (if (>= (fitness new-object)
              (fitness o))
          (setf best-individual new-object)))
    best-individual))

(defmethod prepare-children-from ((o entity-linear-ordering-list) children algorithm)
  "Prepares <o> to behave like <children>."
  (setf (program o) (program children)
        (initial-matrix o) (initial-matrix (initialization-method algorithm))
        (permutations-list o) (permutations-list children)))

(defmethod possible-languages ((o entity-linear-ordering-list))
  (list 
   (system-get 'lop-lists-default-language)))



#|
;; TRASH CODE 4 TEST
(defmethod simplification-1 ((o entity-linear-ordering-list) algorithm)
  "Answer a new individual created with a simplification operation."
  (simplification-iteration o algorithm 10))

(defmethod simplification-iteration ((o entity-linear-ordering-list) algorithm max-iterations)
  "Answer a new individual with a simplified permutations list."
  (let* ((new-object)
         (best-individual (copy o)))
    (do ((i 0 (1+ i)))
        ((>= i (min (structure-size best-individual) max-iterations)))
      (setf new-object (copy best-individual))
      (let ((list (permutations-list new-object)))
        (setf (permutations-list new-object)
              (append (subseq list 0 i)
                      (subseq list (1+ i) (structure-size new-object)))))
      (calculate-matrix new-object)
      (evaluate algorithm new-object)
      (if (>= (fitness new-object)
              (fitness o))
          (setf best-individual new-object)))
    best-individual))

(defmethod lop-crossover-1 ((object-a entity-linear-ordering-list) 
                            (object-b entity-linear-ordering-list)
                            algorithm
                            operator)
  (let ((a (directional-crossover-3 object-a object-b algorithm))
        (b (directional-crossover-3 object-b object-a algorithm)))
    (evaluate algorithm a)
    (if (> (fitness object-b) (fitness a))
        (incf *bad-crossover*)
      (if (= (fitness object-b) (fitness a))
          (incf *equal-crossover*)
        (incf *good-crossover*)))
    (values a b)))

(defmethod permutate-random-row ((o entity-linear-ordering-list) algorithm operator)
  (let ((a (permutate-with o #'permutate-row)))
    (evaluate algorithm a)
    (if (> (fitness o) (fitness a))
        (incf *bad-permutate-row*)
      (if (= (fitness o) (fitness a))
          (incf *equal-permutate-row*)
        (incf *good-permutate-row*)))
    a))

(defmethod permutate-random-column ((o entity-linear-ordering-list) algorithm operator)
  (let ((a (permutate-with o #'permutate-column)))
    (evaluate algorithm a)
    (if (> (fitness o) (fitness a))
        (incf *bad-permutate-column*)
      (if (= (fitness o) (fitness a))
          (incf *equal-permutate-column*)
        (incf *good-permutate-column*)))
    a))
|#