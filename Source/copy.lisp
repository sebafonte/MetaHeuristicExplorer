;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method      : copy                                                      ;;
;; Parameters  : thing                                                     ;;
;; Returns     : A copy of the thing.                                      ;;
;; Description : The dispatcher selects the right method, according to the ;;
;;               type of object to be copied.				   ;;
;; Date        : 3/27/96                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy ((object standard-object))
  (copy-instance object))

;; #TODO: Try to avoid these 3 definitions
(defmethod copy ((object standard-generic-function)) 
  object)

(defmethod copy ((object function)) 
  object)

(defmethod copy ((object t)) 
  object)

;; #TODO: Check
(defmethod copy ((pointer fli::pointer))
  pointer)

(defmethod copy ((list list))
  (cons (copy (car list))
	(copy (cdr list))))

(defmethod copy ((str string))
  str)

(defmethod copy ((sym symbol))
  sym)

(defmethod copy ((num number))
  num)

(defmethod copy ((char character))
  char)

(defmethod copy ((path pathname))
  path)

(defmethod copy ((a array))
  (let ((dimensions (array-dimensions a)))
    (cond
     ((equal (length dimensions) 1)
      (copy-simple-array a))
     ((equal (length dimensions) 2)
      (copy-matrix a))
     ((equal (length dimensions) 3)
      (copy-3-dimensional-matrix a))
     (t
      (error "Can't copy arrays of order higher than three")))))

(defmethod copy ((table hash-table))
  (let ((new-table (make-hash-table :test 'equal)))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-table) (copy value)))
	     table)
    new-table))

(defmethod copy (thing)
  (error "Cannot copy ~&~A~&of type ~A" thing (type-of thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function    : copy-matrix/copy-simple-array                             ;;
;; Parameters  : arr                                                       ;;
;; Returns     : A copy of the array.                                      ;;
;; Description : Copies element by element.                                ;;
;; Date        : 3/27/96                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-matrix (array)
  (let* ((dimensions (array-dimensions array))
	 (new-array (make-array dimensions))
	 (n1 (first dimensions))
	 (n2 (second dimensions)))
    (do ((i 0 (1+ i)))
	((> i (1- n1)))
      (do ((j 0 (1+ j)))
	  ((> j (1- n2)))
	(setf (aref new-array i j) 
	      (copy (aref array i j)))))
    new-array))

(defun copy-simple-array (array)
  (let* ((dimensions (array-dimensions array))
	 (new-array (make-array dimensions))
	 (n (first dimensions)))
    (do ((i 0 (1+ i)))
	((> i (1- n)))
      (setf (aref new-array i) 
	    (copy (aref array i))))
    new-array))

(defun copy-3-dimensional-matrix (array)
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
                (copy (aref array i j k))))))
    new-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function    : copy-instance                                             ;;
;; Parameters  : instance                                                  ;;
;; Returns     : A copy of the instance.                                   ;;
;; Description : Copies slot by slot of the object, returning a different  ;;
;;               instance of the same class, with the same slot values.    ;;
;; Date        : 8/25/94                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-instance (instance)
  (let* ((class (class-name (class-of instance)))
	 (slots (class-slot-names class))
	 (new-object (make-instance class)))
    (dolist (slot-name slots)
      (setf (slot-value new-object slot-name) 
	    (if (slot-boundp instance slot-name)
		(copy (slot-value instance slot-name))
	      nil)))
    new-object))

(defun class-slot-names (class-name)
  "Given a class-name, returns a list with the slots in that class"
  (mapcar #'clos:slot-definition-name
	  (clos:class-slots (find-class class-name))))
