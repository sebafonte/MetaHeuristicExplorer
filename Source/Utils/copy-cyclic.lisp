;; PRE COPY
(defmethod pre-copy ((i t))
  nil)

(defmethod pre-copy ((i standard-object))
  (make-instance (class-name (class-of i))))

(defmethod pre-copy ((i base-model))
  (make-instance (class-name (class-of i)) :forget-defaults t))

;; COPY CYCLIC
(defmethod copy-cyclic ((i t) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i null) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i standard-object) &optional table new-object)
  ;; Create table if not supplied
  (when (not table)
    (setf table (make-hash-table :test 'eq)))
  ;; Answer cached value if found
  (if (gethash i table)
      (return-from copy-cyclic (gethash i table)))
  ;; Copy instance method
  (copy-instance-cyclic i table (pre-copy i)))

;; COPY INSTANCE
(defun copy-instance-cyclic (instance &optional table new-object)
  ;; Add created object to table
  (setf (gethash instance table) new-object)
  ;; Process slots
  (let ((slots (class-slot-names (class-name (class-of instance)))))
    (dolist (slot-name slots)
      (setf (slot-value new-object slot-name) 
	    (if (slot-boundp instance slot-name)
		(copy-cyclic (slot-value instance slot-name) table)
	      nil)))
    new-object))


(defmethod copy-cyclic ((i grammar) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i language) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i object-in-search) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i task-planifier) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i entity-linear-ordering-list) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((list list) &optional table new-object)
  "Allow cyclic references of the objects contained into <list>.
   Dont allow cyclic references of <list> into it."
  (cons (copy-cyclic (car list) table)
	(copy-cyclic (cdr list) table)))

(defmethod copy ((p property))
  "NOTE: Redefined to avoid circular-graph recursion."
  (let ((subject-backup (subject p))
        (update-callback-backup (update-callback p)))
    (setf (subject p) nil
          (update-callback p) nil)
    (let ((c (copy-instance p))) 
      (setf (subject p) subject-backup
            (subject c) subject-backup
            (update-callback p) update-callback-backup
            (update-callback c) update-callback-backup)
      c)))

(defmethod copy-cyclic ((object standard-generic-function) &optional table new-object)  
  object)

(defmethod copy-cyclic ((object function) &optional table new-object) 
  object)

(defmethod copy-cyclic-including ((o t) &optional object new-object)
  "Answer a copy of <o> using <new-object> as a copy for <object>."
  (let ((table (make-hash-table :test 'eq)))
    (setf (gethash object table) new-object)
    (copy-cyclic o table)))

(defmethod copy-arrayed-structure ((a array) &optional table new-object)
  (let ((dimensions (array-dimensions a)))
    (cond
     ((equal (length dimensions) 1)
      (copy-simple-array-cyclic a table))
     ((equal (length dimensions) 2)
      (copy-matrix-cyclic a table))
     ((equal (length dimensions) 3)
      (copy-3-dimensional-matrix-cyclic a table))
     (t
      (error "Can't copy arrays of order higher than three")))))

(defun copy-matrix-cyclic (array &optional table new-object)
  (let* ((dimensions (array-dimensions array))
	 (new-array (make-array dimensions))
	 (n1 (first dimensions))
	 (n2 (second dimensions)))
    (do ((i 0 (1+ i)))
	((> i (1- n1)))
      (do ((j 0 (1+ j)))
	  ((> j (1- n2)))
	(setf (aref new-array i j) 
	      (copy-cyclic (aref array i j) table))))
    new-array))

(defun copy-3-dimensional-matrix-cyclic (array &optional table new-object)
  (let* ((dimensions (array-dimensions array))
	 (new-array (make-array dimensions))
	 (n1 (first dimensions))
	 (n2 (second dimensions))
         (n3 (third dimensions)))
    (do ((i 0 (1+ i)))
	((> i (1- n1)))
      (do ((j 0 (1+ j)))
	  ((> j (1- n2)))
        (do ((k 0 (1+ k)))
            ((> k (1- n3)))
          (setf (aref new-array i j k)
                (copy-cyclic (aref array i j k) table)))))
    new-array))

(defun copy-simple-array-cyclic (array &optional table new-object)
  (let* ((dimensions (array-dimensions array))
	 (new-array (make-array dimensions :element-type (array-element-type array)))
	 (n (first dimensions)))
    (do ((i 0 (1+ i)))
	((> i (1- n)))
      (setf (aref new-array i)
	    (copy-cyclic (aref array i) table)))
    new-array))

(defmethod copy-cyclic ((a array) &optional table new-object)
  (copy-arrayed-structure a table new-object))

(defmethod copy-cyclic ((a string) &optional table new-object)
  (copy a))
