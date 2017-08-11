;; #TODO: Create and move some utilities to another file, clos-utilities.lisp

(defun ttrav (rec &optional (base #'identity))
  "Traverses a tree using a traverse function.
   Example:
     (ttrav #’cons)                            ; our-copy-tree
     (ttrav #’(lambda (l r) (+ l (or r 1))) 1) ; count-leaves
     (ttrav #’nconc #’mklist)                  ; flatten      
     (ttrav #’cons #’identity)
     (funcall (ttrav #'list (lambda (x) (list x))) '(1 2 3))
     (funcall (ttrav #'list (lambda (x) (list x))) '(1 2 3))  "
  (labels ((self (tree)
           (if (atom tree)
               (if (functionp base)
                   (funcall base tree)
                 base)
             (funcall rec (self (car tree))
                      (if (cdr tree)
                          (self (cdr tree)))))))
    #'self))

(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
    (or (rfind-if fn (car tree))
        (if (cdr tree) (rfind-if fn (cdr tree))))))

(defun flatten (tree)
  (if (atom tree)
      (list tree)
    (nconc (flatten (car tree))
           (if (cdr tree) (flatten (cdr tree))))))

(defun select (list condition)
  (let ((result))
    (dolist (element list)
      (if (apply condition (list element))
          (push element result)))
    (nreverse result)))

(defun reject (list condition)
  (let ((result))
    (dolist (element list)
      (if (not (apply condition (list element)))
          (push element result)))
    (nreverse result)))

(defun select-then-collect (list condition transformation)
  (mapcar transformation (select list condition)))

(defun replace-marker (list marker replace-list)
  (funcall (ttrav #'cons 
                  (lambda (x) 
                    (if (equal x marker) replace-list x))) 
           list))

(defun tree-depth (tree)
  (if (consp tree) 
      (+ 1 (if (rest tree) 
               (apply #'max (mapcar #'tree-depth (rest tree))) 
             0)) 
    1))

(defun includes (set element)
  (not (null (find-if (lambda (x) (equal x element)) set))))

(defun includes-equals (set element)
  (not (null (find-if (lambda (x) (equals x element)) set))))

(defun includes-subtree (tree subtree)
  (if (equals tree subtree)
      t
    (if (consp tree)
        (or (includes-subtree (car tree) subtree)
            (includes-subtree (cdr tree) subtree)))))

(defun unique-eql (list)
  (let ((new-list))
    (dolist (i list)
      (if (not (find i new-list))
          (push i new-list)))
    new-list))

(defun unique-equals (list)
  (let ((new-list))
    (dolist (i list)
      (if (not (find i new-list :test #'equals))
          (push i new-list)))
    new-list))

(defun unique (list &optional (test-condition #'equal))
  (let ((new-list))
    (dolist (i list)
      (if (not (find i new-list :test test-condition))
          (push i new-list)))
    new-list))

(defun set-aref (array position element)
  (setf (aref array position) element))

(defun set-aref-2 (array i j element)
  (setf (aref array i j) element))

(defun set-hash (hash-table &rest args)
  (dolist (i (to-pairlist args))
    (setf (gethash (first i) hash-table) (second i))))

(defmethod keys ((table hash-table))
  (let ((keys))
    (maphash (lambda (key value) 
               (declare (ignore value))
               (appendf keys (list key))) table)
    keys))

(defmethod hash-values ((table hash-table))
  (let ((values))
    (maphash (lambda (key value) 
               (declare (ignore key))
               (appendf values (list value))) table)
    values))

(defmethod concrete-subclasses (o)
  "Answer a list with concrete subclasses names of <o>."
  (let ((result))
    (if (not (abstractp o)) (push o result))
    (dolist (i (clos:class-direct-subclasses (find-class o)))
      (setf result (append result (concrete-subclasses (clos:class-name i)))))
    result))

(defmethod abstractp ((o t))
  "Answer whether <o> is abstract."
  nil)

(defmethod abstractp ((o class))
  "Answer whether <o> is abstract."
  (abstractp (class-name o)))

(defun simple-string-to-octets (text)
  (loop for char across text collect (char-code char)))

(defun simple-octets-to-string (vector)
  (loop for char across vector collect (code-char char)))

(defun compress-text (text)
  (salza2:compress-data (simple-string-to-octets text) 'salza2:zlib-compressor))

(defun compress-text-base-64 (text)
  (with-output-to-string (out) 
    (s-base64:encode-base64-bytes (compress-text text) out)))

(defun decompress-text (array)
  (let ((vector (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (result (chipz:decompress nil 'chipz:zlib array)))
    (dotimes (i (length result))
      (let ((char-code (code-char (aref result i))))
        (vector-push-extend char-code vector)))
    vector))

(defun decompress-text-base-64 (text)
  (decompress-text 
   (with-input-from-string (in text)
     (s-base64:decode-base64-bytes in))))

(defun maximum-of (list valuable)
  (let ((max)
        (result))
    (dolist (i list)
      (let ((value (funcall valuable i)))
        (when (or (null max)
                  (> value max))
          (setf max value
                result i))))
    result))

(defun mappapend (function seq)
  (apply #'append (mapcar function seq)))

(defun rad-sin (value)
  (sin (/ (* 2 pi value) 360)))

(defun rad-cos (value)
  (cos (/ (* 2 pi value) 360)))

(defun delete-nth (seq nth)
  (let ((result)
        (counter 0))
    (dolist (i seq)
      (if (not (= counter nth))
          (appendf result (list i)))
      (incf counter))
    result))

(defmacro deletef-nth (seq nth)
  `(setf ,seq (delete-nth ,seq ,nth)))

(defmethod is-kind-of ((object t) (class class))
  (not (null (find class (clos:class-precedence-list (class-of object))))))

(defmethod is-kind-of ((object t) (class symbol))
  (is-kind-of object (find-class class)))

(defun to-pairlist (list)
  (let ((flag 0) 
        (acum) 
        (result))
    (dolist (i list)
      (if (= flag 0)
          (setf acum i flag 1)
        (progn 
          (appendf result (list (list acum i))) 
          (setf flag 0))))
    result))

(defun to-n-paired-list (list n)
  (let ((result)
        (partial-list)
        (counter 0))
    (dolist (i list) 
      (when (>= counter n)
        (appendf result (list partial-list))
        (setf counter 0 partial-list nil))
      (appendf partial-list (list i))
      (incf counter))
    (if partial-list
        (appendf result (list partial-list)))
    result))

(defun internkey (value)
  (intern value "KEYWORD"))

(defmacro while-do (condition &rest body)
  `(do ()
       ((not ,condition))
     ,@body))

(defmacro until-do (condition &rest body)
  `(do ()
       (,condition)
     ,@body))

(defun replace-label-cadr (label list)
  (if (consp list)
      (if (equal (car list) label)
          (replace-label-cadr label (cdr list))
        (cons (replace-label-cadr label (car list))
              (replace-label-cadr label (cdr list))))
    list))

(defun argument-from-key (list key position)
  (let ((index (position key list :test 'equal)))
    (if index (nth (+ index position) list))))

(defun argument-from-key-equal (list key position)
  (let ((index (position-if (lambda (object) (equal object key)) list)))
    (if index (nth (+ index position) list))))

(defun normalize-list (list)
  (let ((acum 0))
    (dolist (i list) (incf acum i))
    (if (zerop acum)
        (values list nil)
      (values (mapcar (lambda (i) (/ i acum)) list)
              t))))

(defun accumulated-list (list)
  "Answer the accumulated values list for <list>."
  (let ((last 0))
    (mapcar (lambda (i) (incf last i)) list)))

(defmethod class-named ((o string) &optional error)
  (class-named (read-from-string o) error))

(defmethod class-named ((o symbol) &optional error)
  (find-class o error))