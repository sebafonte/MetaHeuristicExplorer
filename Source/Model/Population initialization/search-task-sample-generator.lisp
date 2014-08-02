
(defclass search-task-sample-generator (population-generator)
  ((sample-objects :initarg :sample-objects :accessor sample-objects)
   (mutate-individuals :initarg :mutate :accessor mutate-individuals)))


(defmethod initialize-properties :after ((object search-task-sample-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'sample-objects :label "Sample tasks" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value (default-objects object) :editor 'lisp-editor)
   (:name 'mutate-individuals :label "Mutate" :accessor-type 'accessor-accessor-type 
    :data-type 'boolean :default-value nil :editor 'boolean-editor)))

(defmethod generate-population ((object search-task-sample-generator) (algorithm search-algorithm))
  "Generate population for search on <algorithm>."
  (let* ((population-size (population-size algorithm))
         (population (make-array population-size)))
    (dotimes (i (population-size algorithm))
      (let ((new-exp (random-element (sample-objects object))))
        (if (mutate-individuals object)
          (setf new-exp (operate mutation-operator object algorithm)))
        (let ((object (make-instance (objetive-class algorithm) :expresion new-exp)))
          (evaluate algorithm object)
          (setf (aref population i) object))))
    (make-instance 'population :count-individuals population-size :individuals-array population)))

(defmethod default-objects ((object search-task-sample-generator))
  (list
   ;; Simple task
   '(BEST-OF-TASK
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG 40 0 10)
      (MAKE-GN-RND 1)
      (MAKE-FE 1)))
   ;; Composite task (paralell)
   '(BEST-OF-TASKS
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG 40 0 10)
      (MAKE-GN-RND 1)
      (MAKE-FE 1))
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG 40 0 10)
      (MAKE-GN-RND 1)
      (MAKE-FE 1)))
   ;; Composite task (combined results, 2 levels)
   '(BEST-OF-TASKS
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG 40 0 10)
      (MAKE-GN-BESTS (MAKE-TASK
                          (MAKE-BUILDER-IT 10)
                          (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
                          (MAKE-LG 40 0 10)
                          (MAKE-GN-RND 1)
                          (MAKE-FE 1)))
      (MAKE-FE 1)))
   ;; Composite task (combined results, 3 levels)
   '(BEST-OF-TASKS
     (MAKE-TASK
      (MAKE-BUILDER-IT 10)
      (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
      (MAKE-LG 40 0 10)
      (MAKE-GN-BESTS (MAKE-TASK
                          (MAKE-BUILDER-IT 5)
                          (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
                          (MAKE-LG 40 0 10)
                          (MAKE-GN-BESTS (MAKE-TASK
                                              (MAKE-BUILDER-IT 5)
                                              (MAKE-ALG-GG 100 50 (MAKE-SM-TOURNAMENT 3) (MAKE-EM 4))
                                              (MAKE-LG 40 0 10)
                                              (MAKE-GN-RND 1)
                                              (MAKE-FE 1)))
                          (MAKE-FE 1)))
      (MAKE-FE 1)))))