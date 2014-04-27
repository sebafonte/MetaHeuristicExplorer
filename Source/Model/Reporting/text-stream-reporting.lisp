(defclass reporter (object-with-properties)
  ((options :initarg :options :accessor options)))

(defclass text-stream-reporter (reporter)
  ((output-stream :initarg :output-stream :accessor output-stream)))


(defmethod report-on-text-stream ((r text-stream-reporter) (task search-task))
  (let ((ostream (output-stream r)))
    (format ostream "TASK: ~A~%~%" (name task))
    (report-properties ostream 
                       task
                       'description 'best-fitness 'seed 'task-planifier 'objetive-class 
                       'initial-time 'final-time 'running-time)
    (format ostream "~%BEST TASK INDIVIDUAL DATA: ~%")
    (dolist (subtask (children task))
      (report-on-text-stream r (algorithm subtask)))
    (report-on-text-stream r (language task))
    (report-on-text-stream r (fitness-evaluator task))
    (report-on-text-stream r (best-individual task))
    (format ostream "~%")))
  
(defmethod report-on-text-stream ((r text-stream-reporter) (o task-group))
  (let ((ostream (output-stream r)))
    (format ostream "TASK GROUP: ~A~%~%" (name o))
    (let ((initial-time (map-best (mapcar (lambda (task) (initial-time (first (children task))))
                                          (tasks o))
                                  'min))
          (final-time (map-best (mapcar (lambda (task) (final-time (first (children task))))
                                        (tasks o))
                                'max)))
      (format ostream "Total time: ~A~%" (- final-time initial-time))
      (format ostream "~%BEST GROUP INDIVIDUAL DATA: ~%")
      (let ((bests (mapcar 'best-individual (tasks o))))
        (report-on-text-stream r (best-individual bests)))
      (when (not (and
                  (find :print-tasks (options r))
                  (equal (argument-from-key-equal (options r) :print-tasks 1) nil)))
        (format ostream "~%TASKS DATA: ~%")
        (dolist (task (tasks o))
          (report-on-text-stream r task))
        (format ostream "~%")))))

(defmethod report-on-text-stream ((r text-stream-reporter) (o entity))
  (let ((ostream (output-stream r)))
    (format ostream "Fitness: ~A Program: ~A~%" (fitness o) (program o))))

(defmethod report-on-text-stream ((r text-stream-reporter) (o entity-function))
  (let ((ostream (output-stream r)))
    (format ostream "Fitness: ~A Size: ~A Program: ~A~%" 
            (fitness o) (get-value-for-property-named o 'size) (program o))))
  
(defmethod report-on-text-stream ((r text-stream-reporter) (o object-with-properties))
  (dolist (p (properties o))
    (report-on-text-stream r (get-value-for-property o p))))
  
(defmethod report-on-text-stream ((r text-stream-reporter) (o t))
  (let ((ostream (output-stream r)))
    (format ostream "~A~%" o)))

(defmethod report-on-text-stream ((r text-stream-reporter) (o language))
  (let ((ostream (output-stream r)))
    (format ostream "~%LANGUAGE: ~A~%" (name o))
    (report-properties ostream 
                       o
                       'name 'operators 'simplification-function 'simplification-patterns)))

(defmethod report-on-text-stream ((r text-stream-reporter) (o tree-language))
  (let ((ostream (output-stream r)))
    (format ostream "~%TREE LANGUAGE: ~A~%" (name o))
    (report-properties ostream 
                       o
                       'name 'operators 
                       'simplification-function 'simplification-patterns 'max-size 'max-depth)))

(defmethod report-on-text-stream ((r text-stream-reporter) (o generational-algorithm))
  (let ((ostream (output-stream r)))
    (format ostream "~%ALGORITHM (GENERATIONAL): ~A~%" (name o))
    (report-properties ostream 
                       o
                       'name 'max-generations 'generation  
                       'selection-method 'initialization-method
                       'population-size)
    (report-on-text-stream r (population o))
    (report-on-text-stream r (elite-manager o))))

(defmethod report-on-text-stream ((r text-stream-reporter) (o steady-state-algorithm))
  (let ((ostream (output-stream r)))
    (format ostream "~%ALGORITHM (STEADY STATE): ~A~%" (name o))
    (report-properties ostream 
                       o
                       'name 'max-iterations 'iteration 'population-size 'population 
                       'replacement-strategy 'initialization-method)
    (report-on-text-stream r (elite-manager o))
    (report-on-text-stream r (population o))))

(defmethod report-on-text-stream ((r text-stream-reporter) (o elite-manager))
  (let ((ostream (output-stream r)))
    (format ostream "~%ELITE MANAGER:~%")
    (report-properties ostream 
                       o
                       'max-size)
    (dolist (individual (elites o))
      (report-on-text-stream r individual))))
  
(defmethod report-on-text-stream ((r text-stream-reporter) (o population))
  (when (not (and
              (find :print-populations (options r))
              (equal (argument-from-key-equal (options r) :print-populations 1) nil)))
    (let ((ostream (output-stream r)))
      (format ostream "~%POPULATION:~%")
      (dolist (individual (individuals o))
        (report-on-text-stream r individual)))))
   
(defmethod report-on-text-stream ((r text-stream-reporter) (o entity-evaluator))
  (let ((ostream (output-stream r)))
    (format ostream "~%EVALUATOR ~A:~%" (name o))
    (report-properties ostream 
                       o
                       'samples
                       'fitness-function
                       'min-fitness
                       'max-fitness
                       'solution-fitness)))

(defmethod report-on-text-stream ((r text-stream-reporter) (o entity-function-x-evaluator))
  (let ((ostream (output-stream r)))
    (format ostream "~%REGRESSION EVALUATOR ~A:~%" (name o))
    (report-properties ostream 
                       o
                       'target-program
                       'fitness-function
                       'min-fitness
                       'max-fitness
                       'solution-fitness)))
  
(defmethod report-on-text-stream ((r text-stream-reporter) (o entity-vrp-evaluator))
  (let ((ostream (output-stream r)))
    (format ostream "~%VRP EVALUATOR ~A:~%" (name o))
    (report-properties ostream 
                       o
                       'min-fitness
                       'max-fitness
                       'solution-fitness
                       'cities-description
                       'demand-description
                       'max-capacity
                       'max-distance
                       'infeasible-penalty
                       'back-to-depot)))
