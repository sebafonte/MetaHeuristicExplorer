
(defclass property-editor ()
  ((property :initarg :property :initform nil :accessor property)))


(defmethod (setf property) ((pe property-editor) value)
  "Set <value> into property <pe> slot."
  (setf (slot-value pe 'property) value))

(defmethod update-value ((pe property-editor) (object base-model))
  "Updates the property associated with <pe> into <object>."
  (let ((old-value (handler-case (get-value-for-property object (property pe)) (error (function) nil))))
    (when (and (not (read-only (property pe))) (not (eql (value pe) old-value)))
      (set-value-for-property object (property pe) (value pe)))))

(defmethod update-value :after ((pe property-editor) (object editable-probability-object-list-wrapper))
  "Updates the property associated with <pe> into <object>."
  (update object))


;; GUI editor classes
(defclass number-editor (capi:text-input-pane property-editor) ())
(defclass one-line-lisp-editor (capi:text-input-pane property-editor) ())
(defclass lisp-editor (capi:text-input-pane property-editor) ())
(defclass text-editor (capi:text-input-pane property-editor) ())
(defclass symbol-editor (capi:text-input-pane property-editor) ())
(defclass list-editor (capi:option-pane property-editor) ())
(defclass boolean-editor (capi:check-button property-editor) ())
(defclass list-check-selector-editor (capi:check-button property-editor) ())


(capi:define-interface interface-check-list-editor ()
  ()
  (:panes (check-button capi:check-button))
  (:layouts (column-layout capi:column-layout '(check-button)))
  (:default-initargs :layout 'column-layout :title ""))


(defclass button-editor (capi:push-button property-editor) 
  ((subject :initarg :subject :initform nil :accessor subject)))

(defclass file-prompter-editor (button-editor) 
  ((file-path :initarg :file-path :initform nil :accessor file-path)))

(defclass parameter-editor (capi:slider property-editor) 
  ((subject :initarg :subject :initform nil :accessor subject)
   (property :initarg :property :initform nil :accessor property)))

(defclass list-check-editor (property-editor interface-check-list-editor)
  ((possible-values :initarg :possible-values :initform nil :accessor possible-values)
   (selections :initarg :selections :initform nil :accessor selections)
   (subject :initarg :subject :initform nil :accessor subject)
   (property :initarg :property :initform nil :accessor property)))


(defmethod initialize-instance :after ((object list-check-editor) &key selections possible-values)
  "Initialize <object>."
  (declare (ignore selections possible-values))
  (let* ((pane-description (list-check-editor-checks-description object))
         (column-layout (make-instance 'capi:column-layout :description pane-description)))
    (capi:execute-with-interface
     object
     (lambda (&rest args) 
       (declare (ignore args))
       (setf (capi:titled-object-title object) (label (property object))
             (capi:pane-layout object) column-layout)))))

(defmethod list-check-editor-checks-description ((object list-check-editor))
  "Anser a list of editors for <object>."
  (mapcar (lambda (o)
            (make-instance 'capi:check-button 
                           :text (format nil "~A" o) 
                           :data o
                           :selected (find o (get-value-for-property (subject object) (property object)))))
          (possible-values object)))

(defmethod interface-selections ((object list-check-editor))
  "Answer the selected items of <object>."
  (let ((result))
    (dolist (item (capi:layout-description (capi:pane-layout object)))
      (if (capi:button-selected item)
          (appendf result (list (capi:item-data item)))))
    result))
    
(defmethod possible-editor-types ()
  "Answers all posible accessor type for properties."
  '(number-editor
    text-editor
    symbol-editor
    list-editor
    boolean-editor
    one-line-lisp-editor
    lisp-editor
    parameter-editor
    editable-parameter-editor
    object-list-probability-editor
    object-list-selector-editor
    configurable-copy-list-editor
    file-prompter-editor
    list-check-editor
    button-editor
    list-structure))


;;; Editor creation functions.
;;;
;;; #TODO: - Remove the use of types
;;;	   - Add update logic to editors
;;; 

(defmethod create-editor-for-editor-class ((editor-class t) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance 'lisp-editor 
                 :title (label p) 
                 :text (list-to-string (get-value-for-property o p)) 
                 :enabled (not (read-only p))))

(defmethod create-editor-for-editor-class ((editor-type (eql 'number-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-type 
                 :title (label p) 
                 :text (list-to-string (get-value-for-property o p)) 
                 :enabled (not (read-only p))))

(defmethod create-editor-for-editor-class ((editor-class (eql 'one-line-lisp-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-class 
                 :title (label p) 
                 :text (format nil "~A" (list-to-string (get-value-for-property o p)))
                 :enabled (not (read-only p))))

(defmethod create-editor-for-editor-class ((editor-class (eql 'text-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-class 
                 :title (label p) 
                 :text (format nil "~A" (get-value-for-property o p))
                 :enabled (not (read-only p))))

(defmethod create-editor-for-editor-class ((editor-class (eql 'lisp-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (create-editor-for-editor-class 'one-line-lisp-editor p o))

(defmethod create-editor-for-editor-class ((editor-class (eql 'symbol-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-class 
                 :title (label p) 
                 :text (string-downcase (format nil "~A" (get-value-for-property o p)))
                 :enabled (not (read-only p))))

(defmethod create-editor-for-editor-class ((editor-class (eql 'list-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-class 
                 :title (label p) 
                 :items (possible-values p) 
                 :selected-item (get-value-for-property o p) 
                 :enabled (not (read-only p))
                 :font (gp:make-font-description :size 7)))

(defmethod create-editor-for-editor-class ((editor-class (eql 'boolean-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-class 
                 :text (label p) 
                 :selected (get-value-for-property o p) 
                 :enabled (not (read-only p))))

(defmethod create-editor-for-editor-class ((editor-class (eql 'button-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (make-instance editor-class 
                 :title (label p) 
                 :text "" 
                 :callback (lambda (interface data) 
                             (declare (ignore data))
                             (if (get-value-for-property o p) 
                                 (open-in-new-editor (get-value-for-property o p) interface)))
                 :callback-type :interface-data
                 :subject o))

(defmethod create-editor-for-editor-class ((editor-class (eql 'parameter-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (let ((initial-value (to-slider-value (get-value-for-property o p) p)))
    (make-instance 'parameter-editor 
                   :title (format nil "~A (~A,~A)" (label p) (min-value p) (max-value p))
                   :start 0 
                   :end 100 
                   :slug-start initial-value
                   :slug-end (1+ initial-value)
                   :enabled (not (read-only p))
                   :property p
                   :subject o)))

(defmethod create-editor-for-editor-class ((editor-class (eql 'check-list-editor)) (p property) (o base-model))
  (make-instance 'list-check-editor
                 :possible-values (possible-values p)
                 :selections (get-value-for-property o p)
                 :property p
                 :subject o))

(defmethod slider-value-for (value (p property))
  "Convert value of <p> to a slider normalized value."
  (+ (min-value p)
     (* (/ value 100) 
        (- (max-value p) (min-value p)))))

(defmethod to-slider-value (value (p property))  
  "Convert value of <p> to a slider normalized value."
  (round (* 100 (/ (- value (min-value p))
                   (- (max-value p) (min-value p))))))
  
(defmethod create-editor-for-editor-class ((editor-class (eql 'file-prompter-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it큦 data type."
  (let ((editor (make-instance editor-class :title (label p) :text "" :subject o :callback-type :interface-data)))
    (setf (capi:button-press-callback editor) 
          (lambda (interface data) 
            (declare (ignore data interface))
            (setf (file-path editor)
                  (capi:prompt-for-file "Load file"
                                        :filter "*.*"
                                        :operation :open
                                        :filters `("All files" "*.*")))))
    editor))

(defmethod value ((editor number-editor))
  "Answer the value with expected data type represented in <editor>."
  (handler-case (read-from-string (capi:text-input-pane-text editor))
    (error (function) nil)))

(defmethod value ((editor one-line-lisp-editor))
  "Answer the value with expected data type represented in <editor>."
  (read-from-string (capi:text-input-pane-text editor)))

(defmethod value ((editor lisp-editor))
  "Answer the value with expected data type represented in <editor>."
  (read-from-string (capi:text-input-pane-text editor)))

(defmethod value ((editor symbol-editor))
  "Answer the value with expected data type represented in <editor>."
  (read-from-string (capi:text-input-pane-text editor)))
  
(defmethod value ((editor list-editor))
  "Answer the value with expected data type represented in <editor>."
  (capi:choice-selected-item editor))

(defmethod value ((editor boolean-editor))
  "Answer the value with expected data type represented in <editor>."
  (capi:button-selected editor))

(defmethod value ((editor button-editor))
  "Answer the value with expected data type represented in <editor>."
  (get-value-for-property (subject editor) (property editor)))

(defmethod value ((editor parameter-editor))
  "Answer the value with expected data type represented in <editor>."
  (slider-value-for (capi:range-slug-start editor) (property editor)))

(defmethod value ((editor file-prompter-editor))
  "Answer the value with expected data type represented in <editor>."
  (file-path editor))

(defmethod value ((editor list-check-editor))
  "Answer the value with expected data type represented in <editor>."
  (interface-selections editor))

(defmethod value ((editor text-editor))
  "Answer the value with expected data type represented in <editor>."
  (capi:text-input-pane-text editor))