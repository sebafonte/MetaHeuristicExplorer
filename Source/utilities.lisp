(defvar *seed* 53)


(defun park-miller-randomizer ()
  "The Park-Miller multiplicative congruential randomizer (CACM, October 88, Page 1195).  Creates pseudo 
   random floating point numbers in the range 0.0 < x <= 1.0.  The seed value for this randomizer is called 
   *seed*, so you should record/set this if you want to make your runs reproducible.
  #NOTE: 16807 is (expt 7 5) and 2147483647 is (- (expt 2 31) 1)."
  (declare (optimize (safety 0) (speed 3)))
  #+Lucid (unless (typep *seed* 'integer) (setq *seed* (round *seed*)))
  (let ((multiplier #+Lucid 16807 #-Lucid 16807.0d0)
        (modulus #+Lucid 2147483647 #-Lucid 2147483647.0d0))
    (let ((temp (* multiplier *seed*)))
      (setf *seed* (mod temp modulus))
      (#+lucid float #-lucid progn (/ *seed* modulus)))))

(defun random-real (min max)
  "Answer a real random number between <min> and <max>."
  (declare (number min) (number max))
  (coerce (+ min (* (park-miller-randomizer) (- max min))) 'single-float))

(defun random-integer (min max)
  "Answer a real integer number between <min> and <max>."
  (declare (number min) (number max))
  (+ min (floor (* (park-miller-randomizer) (- max min)))))

(defun random-bit ()
  (random-integer 0 2))

(defun /- (a b) 
  "Answer the value for the protected division of <a> by <b>."
  (declare (number a) (number b))
  (if (= b 0) 0 (/ a b)))

(defun plog (x) 
  "Anaswer the value for the protected logarithm of <x>."
  (declare (number x))
  (if (> x 0) (log x) 0))

(defun abs-dif (a b)
  "Answer the abs value of <a> - <b>."
  (declare (number a) (number b))
  (abs (- a b)))

(defun sqr (x)
  "Answer <x> squared."
  (declare (number x))
  (* x x))

(defun real-sqrt (x)
  "Answer <x> square root abs."
  (abs (sqrt x)))

(defun real-expt (x y)
  (let ((value (handler-case (expt x y)
                 (error (function) 0))))
    (if (realp value)
        value
      0)))

(defun crop (min max value)
  "Answer <value> cropped to <min>, <max>."
  (declare (number min) (number max) (number value))
  (cond ((< value min) min)
        ((> value max) max)
        (t value)))

(defun crop-0-1 (value)
  "Answer <value> cropped to [0, 1]."
  (declare (number value))
  (cond ((< value 0) 0)
        ((> value 1) 1)
        (t value)))

(defun find-subtree (tree subexp)
  "Answer whether <subexp> has been found in <tree>."
  (if tree
    (if (equal tree subexp)
        t
      (if (consp tree)
          (or (find-subtree (car tree) subexp)
              (find-subtree (cdr tree) subexp))))))

(defmethod to-array ((l list))
  "Answer an array with <l> contents."
  (let* ((size (length l))
         (array (make-array size)))
    (dotimes (i size) 
      (setf (aref array i) (nth i l)))
    array))

(defmethod to-list ((a array))
  "Answer a new list with <a> contents."
  (map 'list #'(lambda (x) x) a))

(defmethod to-list-without-nils ((a array))
  "Anwer a new list with <a> contents but nils."
  (remove nil (map 'list #'(lambda (x) x) a)))

(defmethod to-string ((object t))
  "Answer a string representation of <object>."
  (string object))

(defmethod to-string ((object number))
  "Answer a string representation of <object>."
  (format nil "~A" object))

(defun list-to-string (x)
  "Answer a new list as a <x> copy, but with each element transformed to string."
  (if x 
      (if (consp x) 
          (cons (list-to-string (car x)) 
                (list-to-string (cdr x)))
        (if (keywordp x)
            (format nil ":~A" x)
          (format nil "~D" x)))))

#|
(defun tree-size (tree)
  (if (listp tree) (reduce #'+ (mapcar #'tree-size tree)) 1))
|#

;; #OPTIMIZED: Fastest version on LispWorks i tested
(defun tree-size (tree)
  (declare (optimize (speed 3) (safety 0)))
  (if (listp tree) 
      (let ((value 0))
        (dolist (i tree)
          (incf value (tree-size i)))
        value)
    1))

(defun select-ranking-index-list (list value-function)
  (let ((result)
        (sum-ranking 0)
        (size (length list)))
    (dotimes (i size)
      (incf sum-ranking (funcall value-function list i (nth i list))))
    (let ((sum 0.0) 
          (value (random-real 0 sum-ranking)))
      (setf result 
            (do ((i 0 (1+ i)))
                ((or (>= i size) (>= (+ sum (funcall value-function list i (nth i list))) value))
                 i)
              (incf sum (funcall value-function list i (nth i list))))))
    (nth result list)))

(defun random-element (list) 
  "Answer a <list> random element."
  (nth (random (list-length list)) list))

(defun random-element-index (list) 
  "Answer a <list> random element and index."
  (let ((value (random (list-length list))))
    (values (nth value list) value)))

(defun random-elements (list n) 
  "Answer <n> <list> distinct random elements."
  (let ((selections)
        (length (length list)))
    ;; Create selection list
    (dotimes (i n)
      (appendf selections (list (random-integer 0 (- length i)))))
    ;; Answer selected values
    (mapcar
     (lambda (index) 
       (let ((value (nth index list)))
         (deletef-nth list index)
         value))
     selections)))

(defun random-element-priority-index (list) 
  "Answer a <list> random element."
  (select-ranking-index-list list 'lambda-weight-for-index-random-selection-list))

(defun random-element-priority-weigth-function (list weigth-function) 
  "Answer a <list> random element using <wight-function>."
  (select-ranking-index-list list weigth-function))

(defmethod save-to-file ((object list) file-path &optional &key (tag "#base-model"))
  "Save <object> into file specified by <file-path> under <tag>."
  (dolist (i object)
    (save-to-file i file-path :tag tag)))

(defmethod load-from-file (file-path &optional &key tag)
  "Loads objects from file specified by <file-path>. 
   Optionally accepts a <tag> argument, used to specify what kind of objects we want to load."
  (let ((return-list))
    (with-open-file (stream file-path)
      (do ((line (read-line stream) (read-line stream nil 'eof)))
          ((eq line 'eof) "Reached end of file.")
        (when (or (null tag) (equal tag line))
          (setf line (read-line stream))
          (push line return-list))))
    return-list))

(defun load-object-from (path)
  (if (probe-file path)
      (eval (read-from-string (car (load-from-file path))))))

(defmethod load-lop-matrix-description (file-path)
  "Loads objects from <file-path>."
  (with-open-file (stream file-path)
    (let* ((size (read-from-string (format nil "(~A)" (read-line stream))))
           (result size))
      (dotimes (i (car size))
        (appendf result (read-from-string (format nil "(~A)" (read-line stream)))))
      result)))

(defmethod equals ((a t) (b t))
  "Answers whether <a> equals <b>."
  (equal a b))

(defun cvec3 (r g b)
  "Answers a new instance of image-vector-3d with <r>, <g> and <b> components."
  (make-instance 'image-vector-3d :x r :y g :z b))

(defmethod name ((o standard-method))
  (format nil "~A" (clos:generic-function-name (slot-value o 'generic-function))))

(defmethod name-symbol ((o standard-method))
  (clos:generic-function-name (slot-value o 'generic-function)))

(defun power-of-2 (n)
  (let ((answer+1 (power-of-2+1 n)))
    (if (numberp answer+1)
      (1- answer+1))))

(defun next-power-of-2 (n)
  (expt 2 (ceiling (log n 2))))

(defun power-of-2+1 (n)
  (cond
   ((or (not (numberp n)) (not (plusp n)) (< n 1))
    nil)
   ((= n 1) 1)
   (t
    (let ((local-answer (power-of-2+1 (/ n 2))))
      (if (numberp local-answer)
        (1+ local-answer))))))

(defun normalize-gradient (g)
  (let ((modulo (sqrt (reduce '+ (map 'vector (lambda (x) (* x x)) g)))))
    (dotimes (i (length g))
      (setf (aref g i) (/- (aref g i) modulo))))
  g)

(define-modify-macro multf (&optional (delta 1)) * "Like <incf>")
(define-modify-macro divf (&optional (delta 1)) / "Like <incf>")

(defun copy-vector (from-vector to-vector &key (start 0) (end (length from-vector)))
  "Given
   [1] from-vector (required) ==> a vector (actually, works with any sequence)
   [2] to-vector (required) ==> a vector (actually, works with any sequence)
   [3] start (keyword; 0) ==> start index of from-vector to be transferred
   [4] end (keyword; length of from-vector) ==> 1 + end index of from-vector to be 
       transferred returns
   [1] to-vector, with elements start to end - 1
       from-vector copied into corresponding
       elements 0 to end - start - 1 of to-vector

   Note: if to-vector is being created on the spot,
         might consider using CL's copy-seq instead."
  (replace to-vector from-vector :start2 start :end2 end))

(defun my-round-to-3 (number)
  (if number
      (coerce (/ (truncate (* 1000 (coerce number 'float))) 1000) 'float)))

(defun my-round-to-2 (number)
  (coerce (/ (truncate (* 100 (coerce number 'float))) 100) 'float))

(defun normalize-operation-list (list)
  "Answer a copy of weighted list <list> normalized."
  (let ((list (copy list))
        (acum 0))
    (dolist (i list) (incf acum (cadr i)))
    (dolist (i list) (setf (cadr i) (/ (cadr i) acum)))
    list))

(defun string-from-file (path)
  (with-open-file (is path)
    (let ((result ""))
      (do ((c (read-char is) 
              (read-char is nil 'the-end)))
          ((not (characterp c)))
        (setf result (concatenate 'string result (string c))))
      result)))

(defun nan-p (x) 
  "Answer whether <x> is a NAN.
   NOTE: Taken from Chris Dean post, http://comments.gmane.org/gmane.lisp.lispworks.general/6870"
  (/= x x))

(defun nan-replace (x value)
  "Answer <value> if <x> it's NAN, otherwise return <x>."
  (if (nan-p x) value x))
