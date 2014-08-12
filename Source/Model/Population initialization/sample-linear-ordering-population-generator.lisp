
(defclass sample-linear-ordering-population-generator (population-generator)
  ((initial-matrix :initarg :initial-matrix :accessor initial-matrix)
   (initial-matrix-file :initarg :initial-matrix-file :accessor initial-matrix-file)
   (initial-permutations :initarg :initial-permutations :accessor initial-permutations)))


(defmethod initialize-properties :after ((object sample-linear-ordering-population-generator))
  "Initialize <object> properties."
  (add-properties-from-values
   object
   (:name 'initial-matrix :label "Initial matrix" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value nil :editor 'lisp-editor)
   (:name 'initial-matrix-file :label "Initial matrix file" :accessor-type 'accessor-accessor-type 
    :data-type 'object :editor 'file-prompter-editor :setter '(setf initial-matrix-file))
   (:name 'initial-permutations :label "Initial permutations" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :default-value 10 :editor 'integer-editor)))

(defmethod (setf initial-matrix-file) (file (o sample-linear-ordering-population-generator))
  "Sets the initial file string in initial-matrix-file slot of <o>."
  (setf (slot-value o 'initial-matrix-file) file)
  (load-initial-matrix-data o))

(defmethod load-initial-matrix-data ((o sample-linear-ordering-population-generator))
  "Loads the initial matrix data of <o>."
  (when (initial-matrix-file o)
    (setf (initial-matrix o) 
          (lop-matrix-from (load-lop-matrix-description (initial-matrix-file o))))))

(defmethod lop-matrix-from (description)
  (let* ((size (first description))
         (rest (cdr description))
         (array (make-array (list size size))))
    (dotimes (i size)
      (dotimes (j size)
        (setf (aref array i j) (car rest)
              rest (cdr rest))))
    array))

(defmethod generate-population ((o sample-linear-ordering-population-generator) (a search-algorithm))
  "Generate population for search on <a>."
  (let* ((population-size (population-size a))
         (population (make-array population-size))
         (initial-matrix (initial-matrix o)))
    (dotimes (i population-size)
      (let ((object (make-instance (objective-class a) :matrix initial-matrix)))
        (dotimes (i (initial-permutations o))
          (setf object (permutate-random-row object a nil)
                object (permutate-random-column object a nil)))
        (evaluate a object)
        (setf (aref population i) object)))
    (make-instance 'population :individuals-array population)))