
(defclass editable-parameter-editor (editable-parameter-editor-interface property-editor) 
  ((subject :initarg :subject :initform nil :accessor subject)))


(defmethod initialize-instance :after ((object editable-parameter-editor) 
                                       &key selections possible-values start end 
                                       slug-start slug-end title enabled subject)
  "Initialize <object>."
  (let* ((property (property object))
         (slider (slider object))
         (initial-value (to-slider-value (get-value-for-property (subject object) property) 
                                         property)))
    ;; Set button data
    (setf (capi:button-press-callback (editor-button object))
          (lambda (interface data) 
            (declare (ignore data))
            (if (car (subject property))
                (open-in-new-editor (car (subject property)) (editor-button object)))))
    ;; Set slider data
    (setf (capi:range-start slider) start
          (capi:range-end slider) end
          (capi:titled-object-title slider) title
          (capi:range-slug-start slider) slug-start
          (capi:range-slug-end slider) slug-end
          (capi:simple-pane-enabled slider) (not (read-only property)))))

(capi:define-interface editable-parameter-editor-interface ()
  ()
  (:panes 
   (slider parameter-editor :accessor slider)
   (editor-button button-editor :accessor editor-button :text ""))
  (:layouts
   (main-layout capi:row-layout '(slider editor-button)))
  (:default-initargs
   :layout 'main-layout
   :title ""))

;; #TODO: Implement a boolean parameter too
(defmethod create-editor-for-editor-class ((editor-class (eql 'editable-parameter-editor)) (p property) (o base-model))
  "Answer an editor pane for property <p> depending on it´s data type."
  (let ((initial-value (to-slider-value (get-value-for-property o p) p)))
    (make-instance 'editable-parameter-editor 
                   :title (format nil "~A (~A,~A)" (label p) (min-value p) (max-value p))
                   :start 0 
                   :end 100 
                   :slug-start initial-value
                   :slug-end (1+ initial-value)
                   :enabled (not (read-only p))
                   :property p
                   :subject o)))

(defmethod value ((editor editable-parameter-editor))
  "Answer the value with expected data type represented in <editor>."
  (slider-value-for (capi:range-slug-start (slider editor)) (property editor)))