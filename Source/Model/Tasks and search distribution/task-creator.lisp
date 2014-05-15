
(defclass range-generator (base-model)
  ())


(defmethod variations ((o range-generator))
  (error "Subclass responsibility"))

(defmethod abstractp ((o (eql 'range-generator)))
  "Answer whether <o> is abstract."
  t)

(defmethod variations-from-description (description)
  (let ((objects (concrete-subclasses 'range-generator)))
    (dolist (i objects)
      (multiple-value-bind (value property)
          (try-parse-from i description)
        (when property
          (return-from variations-from-description (values value property)))))
    (error "Invalid range object")))


(defclass from-to-range (range-generator)
  ((from :initarg :from :accessor from)
   (to :initarg :to :accessor to)
   (step-by :initarg :step-by :accessor step-by)))


(defmethod try-parse-from ((o (eql 'from-to-range)) value)
  (if (and (position :from value)
           (position :to value)
           (position :step value))
      (values
       (loop for i 
             from (nth (1+ (position :from value)) value)
             to (nth (1+ (position :to value)) value)
             by (nth (1+ (position :step value)) value)
             collect i)
       (car value))
    (values nil nil)))


(defclass values-range (range-generator)
  ((elements :initarg :elements :accessor elements)))


(defmethod try-parse-from ((o (eql 'values-range)) value)
  (if (equal (cadr value) :values)
      (values (cddr value) (car value))
    (values nil nil)))


(defclass task-creator (object-with-properties)
  ((name :initarg :name :initform nil :accessor name)
   (task-name :initarg :task-name :accessor task-name)
   (variations :initarg :variations :accessor variations)))


(defmethod initialize-properties :after ((o task-creator))
  "Initialize <o> properties."
  (add-properties-from-values
   o 
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "Task creator" :editor 'text-editor)
   (:name 'task-name :label "Task name" :accessor-type 'accessor-accessor-type 
    :data-type 'string :default-value "Task" :editor 'text-editor)
   (:name 'variations :label "Variations" :accessor-type 'accessor-accessor-type 
    :data-type 'list :default-value nil :editor 'list-editor)))

(defmethod initialize-instance :after ((o task-creator) &rest args &key variations-description)
  "Initialize <o>."
  (setf (variations o) (combine-variations o variations-description)))

;; #TODO:
(defmethod combine-variations ((o task-creator) variations-description)
  (let ((result))
    (dolist (i variations-description)
      (multiple-value-bind (values property)
          (variations-from-description i)
        (appendf result (list (list property values)))))
    result))

(defmethod create-tasks-on ((creator task-creator) (task search-task))
  (let* ((old-process (process task))
         (variations (variations creator))
         (properties (get-variation-properties variations))
         (values (get-variation-values variations))
         (combinations (get-variation-combinations values))
         (result))
    ;; #TODO: Check if necessary
    (setf (process task) nil)
    (dolist (i combinations)
      (let ((new-task (copy-cyclic task)))
        (setf (name new-task) (format nil "~A~A.task" (name new-task) (length result)))
        (apply-variation creator new-task i properties)
        (appendf result (list new-task))))
    ;; #TODO: Check if necessary
    (setf (process task) old-process)
    result))

(defmethod apply-variation ((creator task-creator) new-task variation properties)
  (dolist (i properties)
    ;; Go to last property of the chain
    (let ((final-object new-task))
      (dolist (j i)
        (if (eql j (car (last i)))
            ;; Set value for property
            (apply-chained-property final-object j variation)
          ;; Move to next object
          (setf final-object (get-value-for-property-named new-task j)))))))

(defmethod apply-chained-property (object property value)
  (if (symbolp property)
      (set-value-for-property-named object property value)
    (if (functionp property)
        (apply property (list object value))
      (error "Bad property"))))

(defun get-variation-properties (variations)
  (mapcar 'car variations))

(defun get-variation-values (variations)
  (mapcar 'cadr variations))

(defun get-variation-combinations (variations)
  (first variations))

(defun save-task-on (creator tasks path)
  (dolist (i tasks)
    (let ((file-name (merge-pathnames (name i) path)))
      (if (probe-file file-name) (delete-file file-name))
      (save-source-description i file-name))))


#|
;; Population size test
(setf creator (make-instance 'task-creator :variations-description '(((algorithm population-size) . (:from 5 :to 500 :step 10)))))
(setf task (make-instance 'search-task))
(save-task-on creator (create-tasks-on creator task) "d:\\temp\\tasks\\")

;; Population size
(setf creator (make-instance 'task-creator :variations-description '(((algorithm population-size) . (:values 5 10 15 20 25)))))
(setf task (make-instance 'search-task))
(save-task-on creator (create-tasks-on creator task) "d:\\temp\\tasks\\")
|#
