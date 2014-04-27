
;;; tree-size
(defun tree-size (tree)
  (if (listp tree) 
      (reduce #'+ (mapcar #'tree-size tree)) 
    1))

(defun tree-size-2 (tree)
  (declare (optimize (speed 3) (safety 0)))
  (if (listp tree) 
      (let ((value 0))
        (dolist (i tree)
          (incf value (tree-size-2 i)))
        value)
    1))

(defun test-tree-size ()
  (let ((tree '(+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y)))))
    (dotimes (i 100000)
      (tree-size tree))))

(defun test-tree-size-2 ()
  (let ((tree '(+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y)))))
    (dotimes (i 100000)
      (tree-size-2 tree))))


;;; replace-list-value
(defun replace-list-value (exp source-index target)
  (let ((index 0)
        (new-list))
    (dolist (i exp)
      (appendf new-list 
               (if (= index source-index)
                   target 
                 (list i)))
      (incf index))
    new-list))

(defun replace-list-value-2 (exp source-index target)
  (declare (optimize (speed 3) (safety 0)))
  (let ((index 0)
        (new-list))
    (declare (type fixnum index))
    (dolist (i exp)
      (appendf new-list 
               (if (= index source-index)
                   target 
                 (list i)))
      (incf index))
    new-list))

(setf *exp* '(+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) y))) y))) (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) y))) y))))) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) y))) y))))

(setf *replace-exp* '(+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) y))) y))) (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y)))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- (+ (+ (* (+ (* x y) (- x y)) y) (- x (+ (* (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))) y) (- x (+ (* (+ (* x y) (- x y)) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) y))) y))))) y) (- x (+ (* x y) (- x y)))))))) (+ (* x (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) (+ (* (+ (* x y) (- x y)) y) (- x y))) (- x y))) y))) y))) y)))

(defun test-replace-list-value ()
  (let ((tree *exp*))
    (dotimes (i 1000000)
      (replace-list-value tree 1000 *replace-exp*))))

(defun test-replace-list-value-2 ()
  (let ((tree *exp*))
    (dotimes (i 1000000)
      (replace-list-value-2 tree 1000 *replace-exp*))))


(time (test-replace-list-value))
(time (test-replace-list-value-2))

(time (test-replace-list-value-2))
(time (test-replace-list-value))
