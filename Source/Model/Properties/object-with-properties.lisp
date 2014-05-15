
(defclass object-with-properties (base-model)
  ((properties-definition :initarg :properties-definition :accessor properties-definition)
   (properties-values :initarg :properties-values :accessor properties-values)))


(defmethod initialize-instance :before ((o object-with-properties) &rest keys)
  "Initialize <o>."
  (setf (properties-definition o) nil
        (properties-values o) (make-hash-table)))

(defmethod re-initialize-properties-for ((o t) (target object-with-properties))
  "Re-initialize <o> properties."
  (remove-all-specific-properties-of o target)
  (initialize-properties-for o target)
  (set-default-property-values target))

(defmethod properties ((o t))
  "Answer <o> properties."
  (append (class-properties-definition o)
          (ephemeral-properties-definition o)))

(defmethod properties ((o object-with-properties))
  "Answer <o> properties."
  (append (class-properties-definition o)
          (properties-definition o)
          (ephemeral-properties-definition o)))

(defmethod class-properties-definition ((o t))
  "Answer <o> class defined properties."
  nil)    

(defmethod ephemeral-properties-definition ((o t))
  "Answer <o> ephemeral properties."
  nil)

;; #TODO: Refactor on accesor type object eql methods
(defmethod get-value-for-property-named ((o base-model) name)
  "Answer value for property named <name> for <o>."
  (if (consp name)
      (get-property-chain-value o name)
    (let ((property (property-named o name)))
      (if property
          (cond ((equal (accessor-type property) 'property-accessor-type) 
                 (gethash name (properties-values o)))
                ((equal (accessor-type property) 'accessor-accessor-type) 
                 (funcall name o))
                ((equal (accessor-type property) 'valuable-accessor-type) 
                 (funcall (compiled-valuable property) o))
                (t nil))))))

(defmethod get-value-for-property-labeled ((o base-model) label)
  "Answer value for property labeled as <label> for <o>."
  (let ((property (property-labeled o label)))
    (if property (get-value-for-property o property))))

(defmethod set-value-for-property-named ((o base-model) property-name value)
  "Set <value> for property named <property-name> of <o>."
  (let ((property (property-named o property-name)))
    (if (and property
             (not (read-only property))
             (equal (accessor-type property) 'accessor-accessor-type))
        ;; #TODO: Write a minimal case
        (let ((setter (setter property)))
          (if (null setter) 
              (setf (slot-value o property-name) value)
            (progn
              (if (symbolp setter) 
                  (eval (list 'setf (list setter o) (quote value))))
              (if (functionp setter) 
                  (funcall setter value o))
              (if (and (listp setter)
                       (= (length setter) 2)
                       (equal (first setter) 'setf))
                  (progn
                    (setf *global-interchange-symbol* value)
                    (eval (list 'setf (list (second setter) o) (quote *global-interchange-symbol*)))
                    (unintern (quote *global-interchange-symbol*)))))))
      ;; Register property value
      (if (equal (accessor-type property) 'property-accessor-type)
          (setf (gethash property-name (properties-values o)) value)))
    ;; When necessary, call update-callback
    (when property
      (if (update-callback property) 
          (apply (update-callback property) (list o property))))))

(defmethod set-default-value-for-property-named ((o object-with-properties) property-name value)
  "Sets the default value to <value> of property named <property-name> to <o> if needed."
  ;; Check for property-accessor type
  (let ((property (property-named o property-name)))
    (if (and (equal (accessor-type property) 'property-accessor-type)
             (null (multiple-value-bind (a b) 
                       (gethash property-name (properties-values o))
                     b)))
        (setf (gethash property-name (properties-values o)) value))
    ;; Check for slot accessor-accessor type
    (if (and (equal (accessor-type property) 'accessor-accessor-type) 
             (or (not (slot-boundp o property-name))
                 (null (slot-value o property-name))))
        (setf (slot-value o property-name) value))))

(defmethod name-of-properties ((o base-model))
  "Answers a list with the <o> property names."
  (mapcar 'name (properties o)))

(defmethod remove-all-specific-properties ((o object-with-properties))
  "Remove <o> properties that are specific (their subject is not <o>)."
  ;; Filter all the properties with subject not equal to <o>
  (setf (properties-definition o)
        (remove-if (lambda (property) (not (equal o (subject property))))
                   (properties-definition o))) 
  ;; Rebuild properties definition and properties values container
  (let ((new-properties (make-hash-table)))
    (dolist (property (properties-definition o))
      (setf (gethash (name property) new-properties)  
            (gethash (name property) (properties-values o))))
    (setf (properties-values o) new-properties)))
    
(defmethod remove-all-specific-properties-of ((to-remove t) (o object-with-properties))
  "Remove all properties from an object that are specific (their subject is not <o>)."
  ;; Filter all the properties with subject not equal to <o>.
  (setf (properties-definition o)
        (remove-if (lambda (property) (equal to-remove (subject property)))
                   (properties-definition o))) 
  ;; Rebuild properties definition and properties values container
  (let ((new-properties (make-hash-table)))
    (dolist (property (properties-definition o))
      (setf (gethash (name property) new-properties)  
            (gethash (name property) (properties-values o))))
    (setf (properties-values o) new-properties)))

(defmethod make-property-unbound ((object object-with-properties) property-name)
  "Reset property named <property-name> value of <object>.
   #NOTE: It could be used to re-initialize dependent properties."
  (let ((property (property-named object property-name)))
    (if property
        (let ((property-name (name property)))
          (ecase (accessor-type property)
            ('accessor-accessor-type (slot-makunbound object property-name))
            ('property-accessor-type (remhash (properties-values object) property-name)))))))

(defmethod copy ((object object-with-properties))
  (let ((copy (copy-instance object)))
    (dolist (p (properties-definition copy))
      (if (equal (subject p) object)
          (setf (subject p) copy)))
    copy))

(defmethod apply-changes ((o object-with-properties))
  "Updates <o> for any possible change."
  nil)
