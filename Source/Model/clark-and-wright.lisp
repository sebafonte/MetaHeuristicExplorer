
#|
REFERENCES:

 - http://osiris.tuwien.ac.at/~wgarn/VehicleRouting/neo/algorithms/clarckWrigth.html
 - http://www.hha.dk/~ath/MAN_SC_MODELS/note_cw_savings.pdf  
 - http://web.mit.edu/urban_or_book/www/book/chapter6/6.4.12.html


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
  (setf (program object) (clark-and-wright-sequential evaluator))
  (evaluate evaluator object)
  (print (fitness object)))

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
  (print (fitness object)))
|#


(defun clark-and-wright-sequential (evaluator)
  "Answer a new individual created using C-W savings heuristic.
   Sequential version implementation (Route Extension)."
  ;; Make initial solution
  (let ((solution (make-initial-cw-savings-solution evaluator)))
    (while-do
     ;; Calculate first saving
     (let ((routes-count (length solution)))
       (block 1
         (dotimes (i routes-count)
           (dotimes (j routes-count)
             (if (not (= i j))
                 (let ((saving (saving-when-merging evaluator solution i j)))
                   (when (and (> saving 0) (is-merge-feasible evaluator solution i j))
                     ;; Add to feasible 
                     (setf solution (merge-routes solution (list i j saving)))
                     (return-from 1 t)))))))))
    ;; Answer merged solution
    solution))

(defun clark-and-wright-parallel (evaluator)
  "Answer a new individual created using C-W savings heuristic.
   Parallel version implementation (Best Feasible Merge)."
  ;; Make initial solution
  (let ((solution (make-initial-cw-savings-solution evaluator)))
    (while-do
     (let* ((routes-count (length solution))
            (savings (make-array (sqr routes-count) :initial-element nil))
            (savings-counter 0))
       ;; Calculate savings
       (dotimes (i routes-count)
         (dotimes (j routes-count)
           (if (not (= i j))
               (let ((saving (saving-when-merging evaluator solution i j)))
                 (when (and (> saving 0) (is-merge-feasible evaluator solution i j))
                   (let ((a (first (route-at solution i)))
                         (b (first (last (route-at solution j)))))
                     ;; Add to savings list 
                     (setf (aref savings savings-counter) (list a b saving))
                     (incf savings-counter)))))))
       ;; Merge routes with best savings when possible
       (sort savings (lambda (a b) (if (and a b) (> (third a) (third b)) a)))
       (when (> savings-counter 0)
         (dotimes (i savings-counter)
           (multiple-value-bind (result flag)
               (merge-when-possible evaluator solution (aref savings i))
             (when flag
               (setf solution result
                     savings-counter 0))))
         (zerop savings-counter))))
    ;; Answer merged solution
    solution))

(defun clark-and-wright-adhoc-1 (evaluator)
  "Answer a new individual created using C-W savings heuristic."
  ;; Make initial solution
  (let ((solution (make-initial-cw-savings-solution evaluator))
        (best-saving))
    (while-do
     (let ((routes-count (length solution)))
       ;; Reset saving flag
       (setf best-saving nil)
       ;; Calculate savings
       (dotimes (i routes-count)
         (dotimes (j routes-count)
           (if (not (= i j))
               (let ((saving (saving-when-merging evaluator solution i j)))
                 (when (and (> saving 0) (is-merge-feasible evaluator solution i j))
                   ;; Add to feasible 
                   (if (or (null best-saving)
                           (> saving (third best-saving)))
                       (setf best-saving (list i j saving))))))))
       ;; Merge routes with best savings when possible
       (when best-saving
         (setf solution (merge-routes solution best-saving))
         t)))
    ;; Answer merged solution
    solution))

(defun make-initial-cw-savings-solution (evaluator)
  "Answer a new initial VRP route for CW savings algorithm."
  (let ((list))
    (dotimes (i (cities-count evaluator))
      (if (not (= i 0))
          (appendf list (list (list i)))))
    list))

(defun merge-when-possible (evaluator tour saving)
  (let ((index-a)
        (index-b))
    ;; Search for source tour
    (block 1 
      (let ((i (first saving))
            (index 0))
        (dolist (subtour tour)
          (when (equal i (first (last subtour)))
            (setf index-a index)
            (return-from 1))
          (incf index))))
    ;; Search for target tour
    (block 2
      (let ((j (second saving))
            (index 0))
        (dolist (subtour tour)
          (when (equal j (first subtour))
            (setf index-b index)
            (return-from 2))
          (incf index))))
    ;; Merge when possible, otherwilse return original tour
    (if (and index-a index-b (is-merge-feasible evaluator tour index-a index-b))
        (values (merge-routes tour (list index-a index-b nil)) t)
      (values tour nil))))
  
(defun merge-routes (solution saving-spec)
  "Merge into <solution> routes referenced by <saving-spec>."
  (let ((a (first saving-spec))
        (b (second saving-spec))
        (route-a)
        (route-b)
        (index 0)
        (routes))
    ;; Catch routes and build list
    (dolist (route solution)
      (if (= index a)
          (setf route-a route)
        (if (= index b)
            (setf route-b route)
          (appendf routes (list route))))
      (incf index))
    ;; Merge routes
    (append routes (list (append route-a route-b)))))

(defun saving-when-merging (evaluator object i j)
  "Answer saving of merging <i> and <j> onto <object>.
    - Assume when merging the first elements correspond to <i>.
    - Assume cost(a b) = cost(b a) for every <object> node."
  (let ((first-point (first (route-at object i)))
        (last-point (first (last (route-at object j)))))
    (- (+ (distance-between evaluator 0 first-point)
          (distance-between evaluator 0 last-point))
       (distance-between evaluator first-point last-point))))
   
(defun route-at (object index)
  (nth index object))










