(defclass entity-linear-ordering (entity)
  ((matrix :initarg :matrix :initform nil :accessor matrix)))


(defmethod initialize-instance :after ((o entity-linear-ordering) &key &allow-other-keys)
  "Initialization for <o>.
    #NOTE: Implemented to allow other keys.
    #TODO: Remove this please :)"
  nil)

(defmethod matrix-size ((o entity-linear-ordering))
  "Answer the matrix size of <o>."
  (first (array-dimensions (matrix o))))

(defmethod sum-value ((o entity-linear-ordering))
  "Answer the fitness value of <o>."
  (let ((size (matrix-size o))
        (matrix (matrix o))
        (value 0))
    (declare (integer value) (integer size))
    (dotimes (i (1- size))
      (do ((j (1+ i) (1+ j)))
          ((>= j size))
        (incf value (aref matrix i j))))
    value))

(defun permutate-row (matrix size source target)
  "Permutates <source> row to <target> row of <matrix>."
  (let ((aux 0))
    (declare (integer aux))
    (dotimes (i size)
      (setf aux (aref matrix target i)
            (aref matrix target i) (aref matrix source i)
            (aref matrix source i) aux))))

(defun permutate-column (matrix size source target)
  "Permutates <source> column to <target> column of <matrix>."
  (let ((aux))
    (declare (integer aux))
    (dotimes (i size)
      (setf aux (aref matrix i target)
            (aref matrix i target) (aref matrix i source)
            (aref matrix i source) aux))))

(defmethod permutate-random-row ((o entity-linear-ordering) algorithm operator)
  "Answer a new instance of <o> with a row permutated matrix."
  (declare (ignore operator))
  (let ((new-matrix (copy (matrix o))))
    (permutate-row new-matrix 
                   (matrix-size o)
                   (random-integer 0 (matrix-size o))
                   (random-integer 0 (matrix-size o)))
    (make-instance 'entity-linear-ordering :matrix new-matrix)))
     
(defmethod permutate-random-column ((o entity-linear-ordering) algorithm operator)
  "Answer a new instance of <o> with a column permutated matrix."
  (declare (ignore operator))
  (let ((new-matrix (copy (matrix o))))
    (permutate-column new-matrix 
                      (matrix-size o) 
                      (random-integer 0 (matrix-size o)) 
                      (random-integer 0 (matrix-size o)))
    (make-instance 'entity-linear-ordering :matrix new-matrix)))

(defmethod default-fitness-evaluators ((o entity-linear-ordering))
  "Answer the default classes that can evaluate <o> fitness."
  (list 
   (system-get 'entity-linear-ordering-evaluator)))

(defmethod default-population-initializer ((o entity-linear-ordering))
  "Answer the default population initializer class name of <o>."
  (system-get 'sample-lop-initializer))

(defmethod compute-object-interface-pixmap-step
           ((o entity-linear-ordering) subtask pixmap width heigth render-precision)
  "Compute pixmap values into <pixmap> of <o>."
  (declare (ignore subtask))
  (let* ((matrix-size (matrix-size o))
         (ajuste-x (/ matrix-size width))
         (ajuste-y (/ matrix-size heigth))
         (array (matrix o))
         (x 0)
         (y 0)
         (value)
         (correction (/ 255 (maximum-value o)))
         (bgra-vector (make-array (* heigth width 4) :element-type '(unsigned-byte 8)))
         (bgra (make-array (list heigth width 4) :element-type '(unsigned-byte 8) :displaced-to bgra-vector))
         (image  (gp:make-image-from-port pixmap 0 0 width heigth))
         (access (gp:make-image-access pixmap image)))
      (declare (number x) (number y) (number ajuste-x) (number ajuste-y)
               (ignore render-precision))
      (dotimes (i width)
        (setf x (* i ajuste-x))
        (dotimes (j heigth)
          (setf y (* j ajuste-y))
          (setf value (ceiling 
                       (my-round-to-2
                        (* (aref array 
                                 (floor (* (my-round-to-2 x) x))
                                 (floor (* (my-round-to-2 y) y)))
                           correction))))
          (setf (aref bgra j i 0) value
                (aref bgra j i 1) value
                (aref bgra j i 2) value
                (aref bgra j i 3) 255)))
      (gp:image-access-pixels-from-bgra access bgra-vector)
      (gp:free-image-access access)
      image))

(defmethod maximum-value ((o entity-linear-ordering))
  "Answer the maximum value element in <o>."
  (let ((array (matrix o))
        (matrix-size (matrix-size o))
        (max-value))
    (dotimes (i matrix-size)
      (dotimes (j matrix-size)
        (if (or (null max-value)
                (> (aref array i j) max-value))
            (setf max-value (aref array i j)))))
    max-value))

(defmethod prepare-children-from ((o entity-linear-ordering) children algorithm)
  "Prepares <o> to behave like <children>."
  (declare (ignore algorithm))
  (setf (matrix o) (matrix children)))

(defmethod possible-languages ((o entity-linear-ordering))
  (list 
   (system-get 'lop-default-language)))

(defmethod drawablep ((o entity-linear-ordering))
  "Answer whether <o> can be displayed on the GUI."
  t)
