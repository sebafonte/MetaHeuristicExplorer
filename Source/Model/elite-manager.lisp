;;; #NOTE: Instances should be used with a copy, because it's state is usually dependent from a search algorithm object state

(defclass elite-manager (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (max-size :initarg :max-size :initform 1 :accessor max-size)
   (value-function :initarg :value-function :initform 'lambda-default-fitness-comparer :accessor value-function)
   (elites :initarg :elites :accessor elites)))


(defmethod initialize-properties :after ((o elite-manager))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :max-value 10000 :default-value "Elite manager" :editor 'string-editor)
   (:name 'max-size :label "Max size" :accessor-type 'accessor-accessor-type 
    :data-type 'integer :min-value 0 :max-value 100 ;; Max-value should be population length
    :default-value 1 :editor 'number-editor :object-parameter t)
   (:name 'elites :label "Elites" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :visible nil)))

(defmethod check-elite ((m elite-manager) (o entity))
  "Check manager <m> for a possible new elite <o>."
  (let ((sorted-list (sort (append (elites m) (list o)) (value-function m))))
    (setf (elites m)
          (subseq sorted-list 0 (min (length sorted-list) (max-size m))))))

(defmethod check-elite ((m elite-manager) (p population))
  "Check manager <m> for a possible new elite from <p>."
  (dolist (i (individuals p))
    (check-elite m i)))

(defmethod clear ((m elite-manager))
  "Clears <m> elites."
  (setf (elites m) nil))

(defmethod better ((m elite-manager) (a entity) (b entity))
  "Answer the best individual between <a> and <b> for <m>."
  (better-than a b))

(defmethod better-than ((a entity) (b entity))
  "Answer whether <a> is better than <b>."
  (lambda-default-fitness-comparer a b))

(defmethod better-than ((a object-in-search) (b object-in-search))
  "Answer whether <a> is better than <b>."
  (better-than (object a) (object b)))

(defmethod best-of ((a entity) (b entity))
  "Answer the best of <a> and <b>."
  (if (better-than a b) a b))

(defmethod best-of ((a object-in-search) (b object-in-search))
  "Answer the best of <a> and <b>."
  (if (best-of (object a) (object b)) a b))

(defmethod better-than-fitness-value ((a entity) fitness-value)
  "Answer whether <a> fitness is better than <fitness-value>."
  (lambda-default-fitness-value-comparer a fitness-value))

(defmethod update-population ((m elite-manager) (p population))
  "This method updates <p> to contain elites of <m>."
  (let* ((individuals (individuals p))
         (replace-list (select
                        (elites m)
                        (lambda (object) (not (includes individuals object))))))
    (when replace-list
      (setf (individuals-array p)
            (to-array (subseq 
                       (sort (append replace-list individuals) (value-function m))
                       0 
                       (length (individuals-array p))))))))

(defmethod check-and-update-population ((m elite-manager) (p population))
  "Check elites and update <p>."
  (check-elite m p)
  (update-population m p))

(defmethod best ((m elite-manager))
  "Answer the best registered object in <m>."
  (first (elites m)))
