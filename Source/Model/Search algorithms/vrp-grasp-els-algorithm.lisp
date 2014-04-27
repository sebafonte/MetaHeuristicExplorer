
(defclass vrp-grasp-els-algorithm (search-algorithm)
  ((name :initarg :name :accessor name)
   (np :initarg :np :accessor np)
   (ni :initarg :ni :accessor ni)
   (nc :initarg :nc :accessor nc)
   (rnnh :initarg :rnnh :accessor rnnh)))


(defmethod initialize-properties :after ((a vrp-grasp-els-algorithm))
  "Initialize <a> properties."
  (add-properties-from-values
   a 
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 'vrp-grasp-els-algorithm :editor 'text-editor)
   (:name 'np :label "Phases (np)" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value '1 :editor 'integer-editor)
   (:name 'ni :label "Iterations (ni)" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value '1 :editor 'integer-editor)
   (:name 'nc :label "Children tours (nc)" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value '1 :editor 'integer-editor)
   (:name 'rnnh :label "RNNH" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :default-value (make-instance 'randomized-nearest-neighbor-generator) 
    :editor 'button-editor)))

;;; Adaption from Bio-inspired Algorithms for the Vehicle Routing Problem, see references.
;;;
;;; np     Number of phases
;;; ni     Number of iterations for ELS
;;; nc     Number of children tours
;;; 
;;;   GRASP     if ni = nc = 1
;;;   ELS       if np = 1
;;;   ILS       if np = nc = 1
;;;   GRASPxILS if nc = 1
;;;

(defmethod search-loop ((a vrp-grasp-els-algorithm) seed)
  "Search method implementation for <a>." 
  (setf *seed* (coerce seed 'double-float))
  (let ((fs)
        (fs-)
        (f*)
        (f~)
        (s)
        (s-)
        (s*)
        (s~)
        (p pmin)
        (t*)
        (t-)
        (evaluator (fitness-evaluator a)))
    ;; Loop #1
    (dotimes (ii (1- np))
      (let ((i (1+ ii)))
        ;; Phase initialization
        (if (= np 1)
            (setf s- (clark-wright-savings-algorithm evaluator))
          (setf s- (randomized-nearest-neighbor evaluator)
                t* (vrp-subtour-split s-)))
        (setf fs- (fitness s-))
        ;; Local search
        (vrp-local-search evaluator s-)
        (setf t- (concatenate-vrp-tour evaluator s-))
        ;; ELS loop
        (dotimes (ij (1- ni))
          (let ((j (1+ ij)))
            (setf f~ fs-)
            ;; CHILDREN generation
            (dotimes (ik (1- nc))
              (let ((k (1+ ik)))
                ;; 
                (setf t* t-)
                (vrp-mutate t* p)
                (vrp-subtour-split t* s)
                (vrp-local-search s)
                ;; Keep fs
                (if (< fs f~)
                    (setf f~ fs
                          s~ s))))
            ;; Keep f~
            (if (< f~ fs-)
                  (setf s- s~
                        t- (concatenate-vrp-tour evaluator s-)
                        p pmin)
              (setf p (min (1+ p) pmax)))))
        ;; Keep fs-
        (if (< fs- f*)
            (setf f* fs-
                  s* s-))))
    s*))


(defun clark-wright-savings-algorithm (evaluator)
  (make-instance 'entity-sample-vrp
                 :expresion (clark-and-wright-parallel evaluator)))

(defun randomized-nearest-neighbor (evaluator)
  "Answer a new solution created using randomized nearest neighbor heuristic method."
  (make-instance 'entity-sample-vrp
                 :expresion (generate (rnnh algorithm) evaluator)))

(defun concatenate-vrp-tour (evaluator object)
  "Concatenate <s> vrp solution to obtain a tsp solution."
  (declare (ignore evaluator))
  (setf (program object)
        (apply 'append (program object))))

(defun vrp-local-search (evaluator s)
  (classical-2-opt evaluator s)
  (crossover-move evaluator s)
  (swap-two-nodes evaluator s)
  (or-opt-move evaluator s)
  (string-exchange evaluator s))

(defun classical-2-opt (s)
  "Executes 2-opt optimization on <s>."
  (2-opt-lexical-optimization s))

(defun insert-depot-copies (route)
  (let ((list))
    (dolist (r route)
      (appendf list (list 0))
      (appendf list r))
    (to-array (append list (list 0)))))

(defun delete-depot-copies (route)
  (let ((new-route)
        (new-subroute))
    (dolist (city (cdr (to-list route)))
      (if (= city 0)
          (progn 
            (appendf new-route (list new-subroute))
            (setf new-subroute nil))
        (appendf new-subroute (list city))))
    new-route))

#|
(defun 2-opt-lexical-optimization (evaluator object gmin)
  "Executes 2-opt lexical optimization on <s>."
  (let* ((costs (costs-matrix evaluator))
         (tour (insert-depot-copies (program object)))
         (n (length tour))
         (i1*)
         (i2*)
         (best-tour)
         (g*))
    (setf gmin 0)
    (dotimes (i1 n)
      (let ((t1 (aref tour i1))
            (t2 (aref tour (1+ i1))))
        (dotimes (a2 (- n 3))
          (let ((i2 (+ a2 3)))
            (let* ((t3 (aref tour i2))
                   (t4 (aref tour (1- i2)))
                   (g (+ (- (aref costs t1 t2) 
                            (aref costs t2 t3))
                         (- (aref costs t3 t4) 
                            (aref costs t4 t1)))))
              (if (> g gmin)
                  (let ((new-tour (copy tour)))
                    (replace-indexes new-tour t1 t2 t3 t4)
                    (if (is-feasible-2-opt evaluator new-tour t1 t2 t3 t4)
                        (setf g* g i1* i1 i2* i2 best-tour new-tour)))))))))
    (if (> g* gmin)
        (list g* i1* i2* best-tour))))
|#

(defun 2-opt-lexical-optimization (evaluator object gmin)
  "Executes 2-opt lexical optimization on <s>."
  (let* ((costs (costs-matrix evaluator))
         (i1*)
         (i2*)
         (best-tour)
         (g*)
         (tour (insert-depot-copies (program object)))
         (n (length tour)))
    (setf gmin 0)
    (dotimes (i1 (1- n))
      (let ((t1 (aref tour i1))
            (t2 (aref tour (1+ i1))))
        (dotimes (a2 (- n 3))
          (let ((i2 (+ a2 3)))
            (let* ((t3 (aref tour i2))
                   (t4 (aref tour (1- i2)))
                   (g (+ (- (aref costs t1 t2) 
                            (aref costs t2 t3))
                         (- (aref costs t3 t4) 
                            (aref costs t4 t1)))))
              (if (> g gmin)
                  (let ((new-tour (copy tour)))
                    (replace-indexes new-tour i1 (1+ i1) i2 (1- i2))
                    (if (is-feasible-2-opt evaluator new-tour t1 t2 t3 t4)
                        (setf g* g i1* i1 i2* i2 best-tour new-tour)))))))))
    (if (and g* (> g* gmin))
        (list g* i1* i2* best-tour))))


(defun 2-opt-lexical-optimization-first-slow (evaluator object gmin)
  "Executes 2-opt lexical optimization on <s>."
  (let* ((costs (costs-matrix evaluator))
         (n (length tour)))
    (setf gmin 0)
    (dotimes (tour (program object))
      (dotimes (i ())
        (dotimes (j ())
          (let ((new-tour (copy tour)))
            (replace-indexes-slow new-tour i j)
            (let ((new-cost (evaluate-cost evaluator new-tour)))
              (if (< new-cost cost)
                  (if (is-feasible-2-opt evaluator new-tour t1 t2 t3 t4)
                      (return new-tour))))))))))

(defun replace-indexes (tour t1 t2 t3 t4)
  (let ((aux (aref tour t2)))
    (setf (aref tour t2) (aref tour t3)
          (aref tour t3) aux)
    tour))

(defun replace-indexes (tour t1 t2 t3 t4)
  (let ((aux (aref tour t2)))
    (setf (aref tour t2) (aref tour t4)
          (aref tour t4) aux)
    tour))


;; #TODO: Avoid this check when edges are on the same subtour.
;;        Try to save it in the object to avoid doing it multiple times.
(defun is-feasible-2-opt (evaluator new-tour t1 t2 t3 t4)
  "Answer whether the 2-opt move for <t1> to <t4> is feasible."
  (and (not (and (or (= t1 0) (= t2 0))
                 (or (= t3 0) (= t4 0))
                 (or (= t2 0) (= t4 0))))
       (evaluate-feasible-tour evaluator (delete-depot-copies new-tour))))

#|
(defun tour-for-index (tour index)
  (let ((subtour 0))
    (dolist (city tour)
      (if (zerop city)
          (incf subtour))))
|#

(defun is-feasible-2-opt* (s t1 t2 t3 t4)
  nil)


(defun vrp-mutate (tour p)
  "Executes mutation operation on <s>."
  nil)

(defun vrp-subtour-split (s)
  "Executes split operation to convert <s> into a vrp solution."
  nil)

(defun crossover-move (s)
  "Executes crossover move optimization on <s>"
  nil)

(defun swap-two-nodes (s)
  ""
  nil)

(defun or-opt-move (s)
  "Executes or-opt optimization on <s>."
  nil)

(defun string-exchange (s)
  ""
  nil)


#|
;;; #TODO: 
;;;  Merge it all on a giant tour
;;;  Split procedure
;;;  See where Bellman algorithm has to be used
;;;  Test 2-OPT and define between BEST-IMPROVEMENT and FIRST-IMPROVEMENT
;;;

;; #TESTCASE 2-opt trivial 1
(let ((evaluator (make-instance 'entity-vrp-evaluator :description "Test 2-opt"))
      (object (make-instance 'entity-sample-vrp)))
  (setf (max-capacity evaluator) 100
        (cities-description evaluator) '((1 4) (2 2) (2 0) (0 0) (0 2))
        (demand-description evaluator) '(0 1 1 1 1)
        (demand-matrix evaluator) (to-array (demand-description evaluator)))
  (initialize-fitness-data evaluator)
  (setf (program object) '((1 2 4 3)))
  (evaluate evaluator object)
  (setf (program object) (2-opt-lexical-optimization evaluator object 0))
  (evaluate evaluator object)
  (print (program object))
  (print (fitness object)))

;; #TESTCASE 2-opt trivial 2
(let ((evaluator (make-instance 'entity-vrp-evaluator :description "Test 2-opt"))
      (object (make-instance 'entity-sample-vrp)))
  (setf (max-capacity evaluator) 100
        (cities-description evaluator) '((0 0) (0 1) (0 2) (0 3))
        (demand-description evaluator) '(0 1 1 1)
        (demand-matrix evaluator) (to-array (demand-description evaluator)))
  (initialize-fitness-data evaluator)
  (setf (program object) '((2 1 3)))
  (evaluate evaluator object)
  (setf (program object) (2-opt-lexical-optimization evaluator object 0))
  (evaluate evaluator object)
  (print (program object))
  (print (fitness object)))

;; #TESTCASE
(let ((evaluator (make-instance 'entity-dvrp-evaluator :description "Test" :max-distance 10000))
      (object (make-instance 'entity-sample-vrp)))
  (setf (max-capacity evaluator) 8
        (cities-description evaluator) '(0 1 2 3 4 5 6)
        (demand-description evaluator) '(0 3 2 4 3 2 2)
        (demand-matrix evaluator) (to-array (demand-description evaluator))
        (costs-matrix evaluator)
        #2a((0  64   58  54  41  58 41)
            (64 0    70  95 103  81 40)
            (58 70    0  36  92 113 81)
            (54 95   36   0  72 112 91)
            (41 103  92  72   0  61 71)
            (58 81  113 112  61   0 41)
            (41 40   81  91  71  41  0)))
  (setf (program object) (clark-and-wright-parallel evaluator))
  (evaluate evaluator object)
  (setf (program object) (2-opt-lexical-optimization evaluator object 0))
  (evaluate evaluator object)
  (print (fitness object)))

;; #TESTCASE
(delete-depot-copies (insert-depot-copies '((1 2) (3) (4 5 6) (7))))
|#