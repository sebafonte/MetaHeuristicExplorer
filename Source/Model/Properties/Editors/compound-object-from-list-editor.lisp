
(defclass compound-object-from-list-editor (capi:push-button property-editor) 
  ((subject :initarg :subject :initform nil :accessor subject)))


(defmethod value ((o compound-object-from-list-editor))
  (subject o))

(defmethod abstractp ((o (eql 'compound-object-from-list-editor)))
  "Answer whether <o> is abstract."
  t)

(defmethod create-editor-for-editor-class ((editor-class (eql 'compound-object-from-list-editor)) (p property) (o object-with-properties))
  (error "Subclass responsibility"))


(defclass object-list-probability-editor (compound-object-from-list-editor) 
  ())

;; Creation
(defmethod create-editor-for-editor-class ((editor-class (eql 'object-list-probability-editor)) (p property) (o object-with-properties))
  (let ((value (get-value-for-property o p)))
    (make-instance editor-class
                   :title (label p)
                   :text "" 
                   :enabled (not (read-only p))
                   :callback (lambda (interface data) 
                               (declare (ignore data))
                               (if (get-value-for-property o p) 
                                   (open-in-new-editor 
                                    (make-instance 'editable-probability-object-list-wrapper 
                                                   :subject (get-value-for-property o p))
                                    interface)))
                   :callback-type :interface-data
                   :subject value)))

(defclass object-list-selector-editor (compound-object-from-list-editor) 
  ())

;; Creation
(defmethod create-editor-for-editor-class ((editor-class (eql 'object-list-selector-editor)) (p property) (o object-with-properties))
  (let ((value (get-value-for-property o p)))
    (make-instance editor-class
                   :title (label p)
                   :text "" 
                   :enabled (not (read-only p))
                   :callback (lambda (interface data) 
                               (declare (ignore data))
                               (if (get-value-for-property o p) 
                                   (open-in-new-editor 
                                    (make-instance 'editable-probability-object-list-wrapper 
                                                   :subject (get-value-for-property o p))
                                    interface)))
                   :callback-type :interface-data
                   :subject value)))
