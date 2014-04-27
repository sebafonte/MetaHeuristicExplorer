
(defclass task-creator (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (variations :initarg :variations :accessor variations)))


(defmethod initialize-properties :after ((o task-creator))
  "Initialize <object> properties."
  (add-properties-from-values
   o 
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "Task creator" :editor 'text-editor)
   (:name 'variations :label "Variations" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'list-editor)))

(defmethod create-tasks ((creator task-creator) (task search-task))
  "Answer the search subtasks for <builder>."
  (let ((result)
        (old-process (process task)))
    ;; #TODO: Check if necessary
    (setf (process task) nil)
    (dolist (i (variation-values creator))
      (let ((new-task (copy-cyclic new-task)))
        (apply-variation-values creator new-task i)
        (appendf result (list (copy-cyclic new-task)))))
    ;; #TODO: Check if necessary
    (setf (process task) old-process)
    result))

(defmethod variation-values ((creator task-creator))
  nil)

(defmethod apply-variation-values ((creator task-creator) new-task variation)
  nil)

(defmethod create-tasks-on ((creator task-creator) (task search-task) path)
  (let ((tasks (tasks creator)))
    (dolist (i tasks)
      (save-task-on path i))))

(defmethod create-tasks-on ((creator task-creator) (task search-task) path)
  nil)

#|

;; Population size test
(make-instance 'task-creator :variations '((population-size (:from 5 :to 500 :step 10))))

;; Population size 
(make-instance 'task-creator :variations '((population-size (:values 5 10 15 20 25))))

|#
