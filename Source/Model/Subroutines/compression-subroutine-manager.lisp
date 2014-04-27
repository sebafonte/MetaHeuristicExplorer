;; Package for runtime defined functions
(defpackage gp-subroutine)


(defclass compression-subroutine-manager (language-subroutine-manager)
  ((subroutines :initarg :subroutines :accessor subroutines)
   (scoring-strategy :initarg :scoring-strategy :accessor scoring-strategy)
   (recursion-strategy :initarg :recursion-strategy :accessor recursion-strategy)
   (replacement-strategy :initarg :replacement-strategy :accessor replacement-strategy)
   (transfer-strategy :initarg :transfer-strategy :accessor transfer-strategy)))


(defmethod initialize-properties :after ((o compression-subroutine-manager))
  "Initialize <o> properties."
  (add-properties-from-values
   o
   (:name 'name :label "Name" :accessor-type 'accessor-accessor-type ;:read-only t
    :default-value "Compression subroutine manager" :data-type 'symbol :editor 'text-editor)
   (:name 'subroutines :label "Subroutines" :accessor-type 'accessor-accessor-type 
    :default-value (make-hash-table :test 'equals) :data-type 'list :editor 'lisp-editor)
   (:name 'subroutines-count :label "Subroutines count" :accessor-type 'valuable-accessor-type 
    :data-type 'integer :editor 'integer-editor :getter 'subroutines-count)
   (:name 'scoring-strategy :label "Scoring strategy" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'configurable-copy-list-editor 
    :default-value (system-get 'default-subroutine-equal-scoring-strategy)
    :possible-values (list (system-get 'default-subroutine-equal-scoring-strategy)
                           (system-get 'default-subroutine-fitness-proportional-scoring-strategy)
                           (system-get 'default-subroutine-frequency-proportional-scoring-strategy)))
   (:name 'recursion-strategy :label "Recursion strategy" :accessor-type 'accessor-accessor-type 
    :editor 'configurable-copy-list-editor :default-value 'no-recursion :data-type 'symbol
    :possible-values '(no-recursion hierarchical-recursion))
   (:name 'replacement-strategy :label "Replacement strategy" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'configurable-copy-list-editor 
    :default-value (system-get 'default-no-limit-subroutine-replacement-strategy)
    :possible-values (list (system-get 'default-no-limit-subroutine-replacement-strategy)
                           (system-get 'default-biased-call-subroutine-replacement-strategy)
                           (system-get 'default-subtree-subroutine-replacement-strategy)))
   (:name 'transfer-strategy :label "Tranfer strategy" :accessor-type 'accessor-accessor-type 
    :data-type 'symbol :editor 'configurable-copy-list-editor 
    :default-value (system-get 'default-task-subroutine-transfer-strategy) 
    :possible-values (list (system-get 'default-task-subroutine-transfer-strategy)
                           (system-get 'default-file-subroutine-transfer-strategy)))))

(defmethod reset ((manager compression-subroutine-manager))
  "Reset <manager> state."
  (unintern-all-subroutines manager)
  (clrhash (subroutines manager)))

(defmethod internalize-function ((manager compression-subroutine-manager) tree)
  (let ((stored-id (gethash tree (subroutines manager))))
    (if stored-id
        (first stored-id)
      (let ((function-id (intern (gensym) (subroutine-call-package))))
        (setf (gethash tree (subroutines manager)) (list function-id 0))
        (eval (list 'defun function-id nil tree))
        function-id))))

(defmethod internalize-function-with-arguments ((manager compression-subroutine-manager) tree arguments-count)
  (let ((stored-id (gethash tree (subroutines manager))))
    (if stored-id
        (first stored-id)
      (let ((function-id (intern (gensym) (subroutine-call-package)))
            (argument-list))
        (dotimes (i arguments-count)
          (appendf argument-list (list (argument-name i))))
        (setf (gethash tree (subroutines manager)) (list function-id arguments-count))
        (eval (list 'defun function-id argument-list tree))
        function-id))))

(defun argument-name (argument-index)
  (intern (format nil "ARG~A" argument-index)))

(defmethod unintern-all-subroutines ((manager compression-subroutine-manager))
  (maphash 
   (lambda (key value) (unintern (first value)))
   (subroutines manager)))

(defmethod subroutines-count ((manager compression-subroutine-manager))
  (if (subroutines manager)
      (length (subroutines-list manager))
    0))

(defmethod subroutines-list ((manager compression-subroutine-manager))
  (let ((list))
    (maphash 
     (lambda (key value) (appendf list (list (first value))))
     (subroutines manager))
    list))

(defmethod subtree-for ((manager compression-subroutine-manager) function-id)
  (let ((subroutine-tree))
    (maphash 
     (lambda (key value) 
       (when (eql function-id (first value) )
         (setf subroutine-tree key)))
     (subroutines manager))
    subroutine-tree))

(defmethod has-subroutine ((manager compression-subroutine-manager) function-id)
  (maphash 
   (lambda (key value) 
     (when (eql function-id (first value))
       (return-from has-subroutine t)))
   (subroutines manager))
  nil)

(defmethod arity-token ((m compression-subroutine-manager) word)
  "Anwer arity token for subroutine named with <word>.
   #NOTE: Serious performance hit for arity higher than 8."
  (let ((value (second (find-if 
                        (lambda (o) (eql (car o) word))
                        (hash-values (subroutines m))))))
    (case value
      (0 :0-ary-operator)
      (1 :1-ary-operator)
      (2 :2-ary-operator)
      (3 :3-ary-operator)
      (4 :4-ary-operator)
      (5 :5-ary-operator)
      (6 :6-ary-operator)
      (7 :7-ary-operator)
      (8 :8-ary-operator)
      (otherwise (intern (format nil "~A-ARY-OPERATOR" value) :keyword)))))

;; #WORKING
(defmethod ensure-no-inifinite-recursion ((manager compression-subroutine-manager) tree)
  (ensure-no-infinite-recursion-using manager object (recursion-strategy object)))

;; #NOTE: Correction introduces biases, when recursion is detected, function is replaced by the next lower 
;;        function-id subroutine.
(defmethod ensure-no-infinite-recursion-using ((manager compression-subroutine-manager) tree id 
                                               (strategy (eql 'no-recursion)))
  "No subroutine calls allowed in <tree>."
  (ensure-non-recursive manager id tree))

;; #NOTE: Correction introduces biases, when recursion is detected, function is replaced by the next lower 
;;        function-id subroutine.
(defmethod ensure-no-infinite-recursion-using ((manager compression-subroutine-manager) tree id 
                                               (strategy (eql 'hierarchical)))
  "Subroutine calls allowed in <tree> for a function with minor id."
  (ensure-function-minor-than manager id tree))

(defmethod is-subroutine-call-node (node)
  "Answer whether <node> is a subroutine call node."
  (and (symbolp node)
       (eql (intern (package-name (symbol-package node)))
            (subroutine-call-package))))

(defun subroutine-call-package ()
  "Answer the packages for subroutines runtime created functions."
  'gp-subroutine)

(defun subroutine-integer-identifier (node)
  "Answer integer part for subroutine identifier in <node>."
  (with-input-from-string (stream (symbol-name node) :start 1) (read stream)))


;; Recursion strategy functions
(defun has-subroutine-call (tree)
  (if (atom tree)
      (is-subroutine-call-node tree)
    (or (has-subroutine-call (car tree))
        (has-subroutine-call (cdr tree)))))

(defun ensure-non-recursive (manager function-id tree)
  ;; Check whether the node is a subroutine call, if so, answer default valid node
  (if (atom tree)
      (if (is-subroutine-call-node tree)
          (valid-default-node manager function-id)
        tree)
    ;; Continue traversing tree list
    (cons (ensure-non-recursive manager function-id (car tree))
          (ensure-non-recursive manager function-id (cdr tree)))))

(defun ensure-function-minor-than (manager function-id tree)
  (if (atom tree) 
      (if (is-subroutine-call-node tree)
          ;; Check node is a subroutine call to possibly replace
          (if (< (subroutine-integer-identifier tree) (subroutine-integer-identifier function-id))
              tree
            (intern (lower-subroutine manager function-id) 
                    (subroutine-call-package)))
        tree)
    ;; Continue traversing tree list
    (cons (ensure-function-minor-than manager function-id (car tree))
          (ensure-function-minor-than manager function-id (cdr tree)))))

(defmethod valid-default-node-no-recursion ((manager compression-subroutine-manager) function-id)
  (valid-default-node (language manager) function-id))

;; #TEMPORAL: Could be used a strategy here
(defmethod valid-default-node-hierarchical ((manager compression-subroutine-manager) function-id)
  (lower-subroutine manager function-id))

;; #TODO: #VER: Neighbor podria ser la inmediata superior, o inferior, u otra seleccionada por una estrategia
(defmethod lower-subroutine ((manager compression-subroutine-manager) function-id)
  (let ((subroutines (keys (subroutines manager)))
        (function-id-value (subroutine-integer-identifier function-id)))
    (if (zerop (length subroutines))
        ;; No subroutines but funtion-id
        (valid-default-node (language manager) function-id)
      ;; There are other subroutines
      (let* ((ids (mapcar (lambda (id) (subroutine-integer-identifier id)) subroutines))
             (filtered-ids (select ids (lambda (id) (< id function-id-value)))))
        (if filtered-ids 
            ;; There are neighbor subroutines
            (let ((nearest-value (apply 'max filtered-ids)))
              (intern (format t "" nearest-value) (subroutine-call-package)))
          ;; No neighbor subroutines
          (valid-default-node (language manager) function-id))))))
  
(defmethod valid-default-node ((language tree-language) function-id)
  (declare (ignore function-id))
  ':tag-replace-subroutine)


   
#|
;;; #TODO: Move to test cases

;; Init
(setf mm (make-instance 'compression-subroutine-manager
                        :language (system-get 'compression-lisp-math-function-xy))
      (manager (system-get 'COMPRESSION-LISP-MATH-FUNCTION-XY)) mm)

;; 1
(internalize-function mm '(+ 1 x))
(internalize-function mm '(+ 2 x))
(internalize-function mm '(+ 3 x))
(internalize-function mm '(+ 3 x))

;; 2
(setf aa '(+ (+ 1 'GP-SUBROUTINE::G37315) x))
(ensure-non-recursive mm 'GP-SUBROUTINE::G99999 aa) 

(setf aa '(+ (+ 1 'GP-SUBROUTINE::G37315) x))
(ensure-function-minor-than mm 'GP-SUBROUTINE::G99999 aa)

;; 3
(compress-1
 '(+ (* x y) (/ 1 2))
 (system-get 'compression-lisp-math-function-xy)
 (system-get 'compress-1))

(expand-1 
 '(+ (GP-SUBROUTINE::G37579) (/ (SIN 1) (SIN (COS 2)))) 
 (system-get 'compression-lisp-math-function-xy)
 (system-get 'expand-1))

(dotimes (i 100)
  (print 
   (compress-1
    '(+ (* (* x x) y) (/ (sin 1) (sin (cos 2))))
    (system-get 'compression-lisp-math-function-xy)
    (system-get 'compress-1))))


(compress-flatten-parenthesis-token-value
 '(:EXPRESION ((:0-ARY-OPERATOR GP-SUBROUTINE::G37161))))

(compress-flatten-parenthesis-token-value
 '(:EXPRESION ((:2-ARY-OPERATOR *) (:EXPRESION (:CONSTANT 1)) (:EXPRESION (:CONSTANT 2)))))

|#
