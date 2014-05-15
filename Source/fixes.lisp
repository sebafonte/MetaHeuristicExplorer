
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
             ((bs expresion) $1)
             ((expresion :1-ary-operator expresion)
              `(,$1 ,$2))
             ((expresion :2-ary-operator expresion expresion)
              `(,$1 ,$2 ,$3))
             ((expresion :3-ary-operator expresion expresion expresion)
              `(,$1 ,$2 ,$3 ,$4))
             ((expresion :constant)
              $1)
             ((expresion :var)
              $1)
             ((expresion :list) 
              (parse *current-grammar* $1))))

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
        (make-instance 'population :count-individuals (number-parents p))
        (slot-value p 'children) 
        (make-instance 'population :count-individuals (number-children p)))
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