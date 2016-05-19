(defclass context-free-grammar (grammar)
  ())


(defmethod is-valid-expression ((g context-free-grammar) expresion) 
  "Answer whether <expresion> is valid for <g>."
  (multiple-value-bind (result error)
      (parse g expresion)
    (declare (ignore result))
    (not error)))

(defun deparse (expression)
  (car (deparse-recursive expression)))

#|
(defun deparse-recursive (expression)
  "Answer final tree without intermediate productions with values."
  (if (atom (cadr expression))
      (list (cadr expression))
    (let ((new-list))
      (if (atom (car expression))
          (progn
            (dolist (i (cdr expression))
              (appendf new-list (deparse-recursive i)))
             new-list)
        (progn 
          (dolist (i expression)
            (appendf new-list (deparse-recursive i)))
          (list new-list))))))
|#

(defun deparse-recursive (expression)
  "Answer final tree without intermediate productions with values."
  (if (and (cadr expression) 
           (atom (cadr expression)))
      (list (cadr expression))
    (let ((new-list))
      (if (atom (car expression))
          (progn
            (dolist (i (cdr expression))
              (appendf new-list (deparse-recursive i)))
             new-list)
        (progn 
          (dolist (i expression)
            (appendf new-list (deparse-recursive i)))
          (list new-list))))))

(defun deparse-type (expression)
  "Answer final tree without intermediate productions with token types."
  (if (consp (cadr expression))
      (let ((new-list))
        (dolist (i (cadr expression))
          (appendf new-list (list (deparse-type i))))
        new-list)
    (car expression)))

(defun flatten-parenthesis (expression)
  "Answer a list for <expression> tree ready to be parsed."
  (if (consp expression)
      (let ((new-list '(:open))
            (new-sub-list))
        (dolist (i expression)
          (let ((value (flatten-parenthesis i)))
            (appendf new-sub-list (if (consp value) value (list value)))))
        (appendf new-list new-sub-list)
        (appendf new-list '(:close))
        new-list)
    expression))

(defun de-flatten-parenthesis (expression)
  "Answer a list for <expression> tree ready to be parsed."
  (car (de-flatten-parenthesis-recursive expression)))

(defun de-flatten-parenthesis-recursive (expression)
  "Answer a list for <expression> tree ready to be parsed."
  (let ((result)
        (acum-list)
        (acumulation 0))
    (dolist (e expression)
      (if (eql e :close)
          ;; Close parenthesis
          (progn 
            (decf acumulation 1)
            (if (= acumulation 0)
                (progn 
                  (appendf result (list (de-flatten-parenthesis-recursive acum-list)))
                  (setf acum-list nil))
              (appendf acum-list (list e))))
        ;; Open parenthesis
        (if (eql e :open)
            (progn 
              (incf acumulation 1)
              (if (= acumulation 1)
                  (setf acum-list nil)
                (appendf acum-list (list e))))
          ;; Other element
          (if (= acumulation 0)
              (appendf result (list e))
            (appendf acum-list (list e))))))
    result))

