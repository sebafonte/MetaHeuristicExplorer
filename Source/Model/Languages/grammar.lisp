(defparameter *infinite-productions-size-value* 1000000)


(defclass grammar (base-model)
  ((name :initarg :name :initform nil :accessor name)
   (productions :initarg :productions :initform nil :accessor productions)
   (updated-productions :initarg :updated-productions :initform nil :accessor updated-productions)
   (tokens :initarg :tokens :initform nil :accessor tokens)
   (minimum-production-sizes 
    :initarg :minimum-production-sizes :initform (make-hash-table) :accessor minimum-production-sizes)
   (lexer :initarg :lexer :accessor lexer)
   (parser-initializer :initarg :parser-initializer :accessor parser-initializer)
   (crossover-tokens :initarg :crossover-tokens :initform nil :accessor crossover-tokens)
   ;; #TEMP: Just pass a keyword, variable is not needed. Anyways take a look, maybe this is used to make copies faster (?)
   (skip-initialization :initarg :skip-initialization :initform nil :accessor skip-initialization)))


(defmethod print-object ((o grammar) seq)
  (format seq "~A" (name o)))

(defmethod copy ((o grammar))
  (make-instance (class-name (class-of o))
                 :name (name o)
                 :productions (copy (productions o))
                 :updated-productions (copy (updated-productions o))
                 :tokens (tokens o)
                 :minimum-production-sizes (copy (minimum-production-sizes o))
                 :lexer (lexer o)
                 :parser-initializer (parser-initializer o)
                 :crossover-tokens (copy (crossover-tokens o))
                 :skip-initialization t))

(defmethod initialize-instance :after ((g grammar) &rest keys) 
  "Initialize <g>."
  (when (not (skip-initialization g))
    (re-generate-parser g)))

(defmethod (setf productions) ((p t) (g grammar))
  "Setter for productions of <g> to <p>."
  (setf (slot-value g 'productions) p)
  (re-generate-parser g))

(defmethod (setf tokens) ((p t) (g grammar))
  "Setter for tokens of <g> to <p>."
  (setf (slot-value g 'tokens) p)
  (re-generate-parser g))

(defmethod re-generate-parser ((g grammar))
  "Regenerate <g> parser."
  (generate-parser g)
  (calculate-minimum-production-size g))

(defmethod generate-parser ((g grammar))
  "Create parser for <g>"
  (funcall (parser-initializer g) (name g)))

(defmethod parse ((g grammar) expression)
  "Answer parsing expression for <expression> parsed with <g>."
  (let* ((*current-grammar* g)
         (linear-expression (flatten-parenthesis expression))
         (*parser-input* (if (consp linear-expression) linear-expression (list linear-expression))))
    (funcall (name g) (lambda () (funcall (lexer g) g)))))

(defun all-keywords-in (list)
  (null (select list (lambda (value) (not (keywordp value))))))

(defun select-random-production (grammar productions production size weight-function)
  (let ((possible (sized-productions-for grammar productions production size)))
    (car (random-element-prioridad-weigth-function possible weight-function))))

(defun productions-for (grammar productions production)
  (declare (ignore grammar))
  (filtered-indexed-list 
   productions 
   (lambda (value)
     (equal (first value) production))))

(defun sized-productions-for (grammar productions production max-size)
  (filtered-indexed-list 
   productions 
   (lambda (value)
     (and (equal (first value) production)
          (<= (calculated-minimum-production-size grammar value)
              max-size)))))

(defun replace-random-production (grammar exp productions max-size weight-function)
  (declare (special size))
  (let* ((source-index (select-random-index-when exp (lambda (value) (not (keywordp value)))))
         (source (nth source-index exp))
         (exp-size (calculated-minimum-production-size grammar (cons 1 exp)))
         (source-size (calculated-minimum-element-size grammar source))
         (size (+ (- max-size exp-size) source-size)))
    (let ((target (cdr (select-random-production grammar productions source size weight-function))))
      (replace-list-value exp source-index target))))

(defun filtered-indexed-list (list condition)
  (let ((index 0)
        (new-list))
    (dolist (i list)
      (if (funcall condition i)
          (appendf new-list (list (list i index))))
      (incf index))
    new-list))

(defun select-random-index-when (list condition)
  (let ((new-list (filtered-indexed-list list condition)))
    (cadr (random-element new-list))))

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

(defun structural-symbol (symbol)
  (or (eql symbol :open)
      (eql symbol :close)))
      
(defun minimum-production-size (grammar all-productions production)
  (if (keywordp production)
      (if (structural-symbol production) 0 1)
    (minimum-non-terminal-size grammar all-productions production)))

(defun minimum-non-terminal-size (grammar all-productions production)
  (let ((p (mapcar 
            (lambda (value) (car value))
            (productions-for grammar all-productions production)))
        (minimum-size *infinite-productions-size-value*))
    (dolist (i p)
      (let ((local-size 0))
        (dolist (j (cdr i))
          (incf local-size (minimum-production-size grammar (remove i all-productions) j)))
        (if (or (null minimum-size)
                (< local-size minimum-size))
            (setf minimum-size local-size))))
    minimum-size))

(defun calculate-minimum-production-size (grammar)
  (setf (minimum-production-sizes grammar) (make-hash-table))
  (let ((productions (updated-productions grammar)))
    (dolist (p productions)
      (setf (gethash (car p) (minimum-production-sizes grammar))
            (minimum-non-terminal-size grammar productions (car p))))))

(defun calculated-minimum-production-size (grammar production)
  (let ((local-size 0))
    (dolist (i (cdr production))
      (incf local-size (calculated-minimum-element-size grammar i)))
    local-size))

(defun calculated-minimum-element-size (grammar element)
  (if (keywordp element)
      (if (structural-symbol element) 0 1)
    (gethash element (minimum-production-sizes grammar))))

(defmethod update-end-productions ((g grammar) tokens functions variables objects)
  (let ((variable-list (mapcar (lambda (var) (list var :var)) variables)))
    (setf (updated-productions g) 
          (append (productions g)
                  (end-productions-for tokens functions)
                  (end-productions-for tokens variable-list)
                  (end-productions-for tokens objects)))))

(defun matching-function-tokens (functions-list tokens)
  (mapcar
   (lambda (a) 
     (list a (find-if 
              (lambda (b) (equal (car b) (car a)))
              functions-list)))
   tokens))

(defun end-productions-for (tokens functions)
  (let ((not-found-table (make-hash-table))
        (result))
    (dolist (i tokens)
      ;; Mark as used if it wasn't
      (setf (gethash (second i) not-found-table) (gethash (second i) not-found-table))
      ;; Add to productions if found, reset
      (if (find-if (lambda (b) (equal (car b) (car i))) functions)
          (progn 
            (appendf result (list (list (intern (second i)) (intern (first i)))))
            (appendf result (list (list (intern (first i)) (intern (second i) :keyword))))
            (setf (gethash (second i) not-found-table) t))))
    ;; Add missing productions
    (dolist (k (keys not-found-table))
      (if (not (gethash k not-found-table))
          (appendf result (list (list (intern k) (intern k))))))
    result))
      