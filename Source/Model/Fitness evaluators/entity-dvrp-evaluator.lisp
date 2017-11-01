
(defclass entity-dvrp-evaluator (entity-vrp-evaluator)
  ((max-distance :initarg :max-distance :accessor max-distance)))

(defmethod initialize-properties :after ((object entity-dvrp-evaluator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'max-distance :label "Max. distance" :accessor-type 'accessor-accessor-type
    :data-type 'number :min-value 0 :max-value 1000000 :default-value 100 :editor 'number-editor)))


;; #NOTE: Clark and wright extension
(defmethod is-merge-feasible ((evaluator entity-dvrp-evaluator) object i j)
  "Answer whether it´s possible to merge routes <i> and <j> onto <object>."
  (let ((first-route (route-at object i))
        (second-route (route-at object j)))
    (and (<= (+ (route-capacity evaluator first-route)
                (route-capacity evaluator second-route))
             (max-capacity evaluator))
         (<= (+ (route-distance evaluator first-route)
                (route-distance evaluator second-route))
             (max-distance evaluator)))))

(defmethod evaluate ((evaluator entity-dvrp-evaluator) (o entity-sample-vrp))
  "Use <evaluator> to calculate and answer <object> fitness."
  (let ((costs-matrix (costs-matrix evaluator))
        (demand-matrix (demand-matrix evaluator))
        (tour (program o))
        (total-cost 0)
        (max-capacity (max-capacity evaluator))
        (max-distance (max-distance evaluator)))
    ;; Execute plan
    (dolist (vehicle-plan tour)
      (let ((actual-city 0)
            (vehicle-distance 0)
            (used-capacity 0))
        ;; Tour cost
        (dolist (next-city vehicle-plan)
          (incf vehicle-distance (aref costs-matrix actual-city next-city))
          (incf used-capacity (aref demand-matrix next-city))
          (setf actual-city next-city))
        ;; Back to depot distance option
        (if (back-to-depot evaluator)
            (incf vehicle-distance (aref costs-matrix actual-city 0)))
        ;; Infeasible solution penalty (capacity)
        (if (or (> used-capacity max-capacity) (> vehicle-distance max-distance))
            (incf total-cost (infeasible-penalty evaluator)))
        (incf total-cost vehicle-distance))
    ;; Assign fitness
    (setf (fitness o) total-cost))))

(defmethod evaluate-feasible-tour ((evaluator entity-dvrp-evaluator) tour)
  "Use <evaluator> to calculate and answer <object> fitness."
  (let ((costs-matrix (costs-matrix evaluator))
        (demand-matrix (demand-matrix evaluator))
        (total-cost 0)
        (max-capacity (max-capacity evaluator))
        (max-distance (max-distance evaluator)))
    (block block-main
      ;; Execute plan
      (dolist (vehicle-plan (delete-depot-copies tour))
        (let ((actual-city 0)
              (vehicle-distance 0)
              (used-capacity 0))
          ;; Tour cost
          (dolist (next-city vehicle-plan)
            (incf vehicle-distance (aref costs-matrix actual-city next-city))
            (incf used-capacity (aref demand-matrix next-city))
            (setf actual-city next-city))
          ;; Back to depot distance option
          (if (back-to-depot evaluator)
              (incf vehicle-distance (aref costs-matrix actual-city 0)))
          ;; Infeasible solution penalty (capacity)
          (if (or (> used-capacity max-capacity) (> vehicle-distance max-distance))
              (return-from block-main nil)))
        ;; No infeasible subtours where found
        t))))