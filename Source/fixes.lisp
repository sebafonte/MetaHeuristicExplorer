
;; Patch to avoid selection on invisible panes 
(defmethod set-image-model ((i interface-pane-editor-entity-base) object image-editor)
  "Set <i> image model."
  (declare (ignore object) (ignore image-editor))
  (setf (pixmap (pane-image i)) nil)
  (when (capi:pane-layout (pane-image i))
    (when (capi:simple-pane-visible-width i)
      (capi:execute-with-interface 
       i
       (lambda (&rest args)
         (declare (ignore args))
         (capi:redraw-pinboard-layout (capi:pane-layout (pane-image i)) 0 0
                                      (capi:simple-pane-visible-width i)
                                      (capi:simple-pane-visible-height i)))))))

;; Patch to avoid #'load-on-demand runtime call for PARSERGEN
(eval
 `(defparser test
             ((start expresion) $1)
             ((expresion :open expresion :2-ary-operator expresion :close)
              `(,$2 ,$1 ,$3))
             ((expresion factor)
              $1)
             ((factor :var) $1)
             ((factor :constant) $1)))

;; Patch to avoid #'load-on-demand runtime call for COMM
(require "comm")

;;
;; FIX for interface, testing whether to include it and how to factorize to the 
;; minimal expression.
;;
(defmethod copy-cyclic ((i object-in-search) &optional table new-object)
  ;; Create table if not supplied
  (when (not table)
    (setf table (make-hash-table :test 'eq)))
  ;; Answer cached value if found
  (if (gethash i table)
      (return-from copy-cyclic (gethash i table)))
  ;; Copy instance method
  (copy-instance-cyclic i table (pre-copy i)))

;; Fix to solve some warning which can molest the buffer locking and speed
(defmethod compiled-program ((o entity-function-x-y))
  "Answer the compiled function for <o>."
  (let ((compiler::*compiler-warnings* nil))
    (compile nil `(lambda () 
                    (declare (special x) (special y) (special *time-variable*)) 
                    ,(program o)))))

;; Copy fixes
(defmethod copy-cyclic ((i context-free-grammar-with-subroutines) &optional table new-object)
  (copy i))

(defmethod copy-cyclic ((i compression-tree-language) &optional table new-object)
  (copy i))

;; Patch to avoid missing behaviour when creating instances / load from serialized 'pane-explorer
;;   The instance is created before the object executes it's most specific intialize-instance :after method.
;;   So initialization code cannot be there, when a pane gets instanciated from serialization, interface is nill during the
;;   execution of that method and we cant trust it should be there.
(defmethod initialize-instance :after ((p pane-explorer) &key key)
  "Initialize <p>."
  (declare (ignore key))
  (initialize-defaults p))

(defmethod initialize-defaults ((p pane-explorer))
  (setf (level (history p)) (get-value-for-property-named p 'history-level)
        (slot-value p 'parents)
        (make-instance 'population :size (number-parents p))
        (slot-value p 'children) 
        (make-instance 'population :size (number-children p)))
  (load-default-configuration p))

(defmethod new-from-description ((o pane-description))
  "Answer a new instance of the pane described by <o>."
  (let ((pane (pane o)))
    (appendf (interface-arguments pane) 
             (list :x (x o) :y (y o) :best-height (heigth o) :best-width (width o)))
    (initialize-interface pane)
    (open-pane pane :mdi-interface (interface *main-pane*))
    (update-pane-interface pane)))

(defun open-pane-explorer (interface data)
  "Open a 'pane-explorer on <interface>."
  (declare (ignore data interface))
  (let ((pane (make-instance 'pane-explorer :mdi-interface (interface *main-pane*) :open nil)))
    (update-pane-interface pane)
    (open-pane pane :mdi-interface (interface *main-pane*))))

(defmethod update-pane-interface ((pane t))
  t)


;; #NOTE: Null parser. Grammars have to be initialized with a parser initializer.
(defun initialize-null-parser (name)
  (declare (ignore name))
  (lambda (&rest args)
    (declare (ignore args))
    (progn nil)))


;; #NOTE: Closed editors cleaning? Comes from working-interfaces.lisp, it could be implemented using events
(defmethod refresh-editors ((o opengl-with-capi))
  (purgue-invisible-editors)
  (dolist (editor *interface-editors*)
    (when (graphic-part editor)  
      (redisplay-canvas (graphic-part editor))))
  (dolist (editor *interface-graphic-editors*)
    (when (graphic-part editor)
      (redisplay-canvas (graphic-part editor)))))

(defun purgue-invisible-editors ()
  (setf *interface-editors* 
        (select 
         *interface-editors*
         (lambda (editor) (capi-internals:representation editor)))))

#|
;; #TODO: delete, not used
(defmethod min-function-args ((o tree-language))
  (when (or (not (slot-boundp o 'tree-language))
            (null min-function-args))
    (setf (min-function-args o) (min-language-function-with-args (functions o)))))

;; Only used to create random, there should not be a need to cache this using cfg
(defmethod (setf functions) (value (o tree-language))
  (setf 
   (slot-value o 'functions) value
   (min-function-args o) (min-language-function-with-args (functions o))))
|#

(defmethod (setf functions) (value (o cfg-tree-language))
  (setf 
   (slot-value o 'functions) value))

;; GLSL corrections found
(defun minimum-production-size (all-productions production &optional passed)
  (if (and (symbolp production) (keywordp production))
      (if (structural-symbol production) 0 1)  
    (let ((q (non-recursive-right-productions-for all-productions (append (list production) passed)))
          (minimum-size *infinite-productions-size-value*))
      (dolist (i q)
        (let ((local-size 0))
          (dolist (j (cdr i))
            (incf local-size (minimum-production-size all-productions j (append (list production) passed))))
          (if (or (null minimum-size)
                  (< local-size minimum-size))
              (setf minimum-size local-size))))
      minimum-size)))

(defmethod calculate-minimum-production-size ((g grammar))
  (setf (minimum-production-sizes g) (make-hash-table))
  (let ((productions (updated-productions g)))
    (dolist (p productions)
      (setf (gethash (car p) (minimum-production-sizes g))
            (minimum-production-size productions (car p))))))

(defun non-recursive-right-productions-for (productions passed)
  (select
   ;; productions-for result
   productions 
   ;; non-recursive production condition
   (lambda (o)
     (and 
      (eql (first passed) (car o))
      (null (intersection passed (cdr o)))))))

(defmethod update-end-productions ((g grammar) tokens functions variables objects)
  ;; #TODO: Refactor using #'variable-tokens
  (let ((variable-list (mapcar (lambda (var) 
                                 (if (listp var)
                                     (list (first var) (second var))
                                   (list var :var))) 
                               variables)))
    (setf (updated-productions g) 
          (filter-recursive-end-productions
           (append (productions g)
                   (end-productions-for tokens functions)
                   (end-productions-for tokens variable-list)
                   (end-productions-for tokens objects))))))

(defmethod updated-definition ((g grammar) tokens functions variables objects)
  (let ((variable-list (mapcar (lambda (var) 
                                 (if (listp var)
                                     (list (first var) (second var))
                                   (list var :var))) 
                               variables)))
    (filter-recursive-end-productions
     (append (definition g)
             (end-definition-for tokens functions)
             (end-definition-for tokens variable-list)
             (end-definition-for tokens objects)))))

(defun end-definition-for (tokens functions)
  (let ((not-found-table (make-hash-table))
        (result))
    (dolist (i tokens)
      ;; Mark as used if it wasn't
      (setf (gethash (second i) not-found-table) (gethash (second i) not-found-table))
      ;; Add to productions if found, reset
      (if (find-if (lambda (b) (equal (car b) (car i))) functions)
          (progn 
            (appendf result (list (list (name-intern (second i)) (name-intern (first i)))))
            (appendf result (list (list (name-intern (first i)) (intern (name-intern (second i)) :keyword))))
            (setf (gethash (second i) not-found-table) t))))
    ;; Add missing productions
    ;; #NOTE: it's neccessary to have an intermediate recursive node for productions n
    (dolist (k (keys not-found-table))
      (if (not (gethash k not-found-table))
          (appendf result (list (list (name-intern k) (name-intern k))))))
    ;(mapcar (lambda (o) (list o (SYSTEM::BQ-LIST :EXP $1))) result)
    ;(mapcar (lambda (o) (list o (append '(SYSTEM::BQ-LIST)
    ;                                    (intern (name-intern (first o)) :keyword)
    ;                                    '($1))))
            result))

(defun filter-recursive-end-productions (list)
  (select
   list
   (lambda (o)
     (not (and (eql (first o) (second o))
               (= 2 (length o)))))))

(defun filter-recursive-end-rules (list)
  (select
   list
   (lambda (o)
     (let ((o (first o)))
       (not (and (eql (first o) (second o))
                 (= 2 (length o))))))))

