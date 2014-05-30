
(defclass test-dependant-properties (test-base-model) ())


(defmethod test-dependant-property ((o test-properties))
  "Verifies whether inheritance of properties behaves correctly."
  (let* ((object (make-instance 'test-properties-dependant-class-a)))
    (check (valid-value-p object property))
    (set-value-for-property object property 'entity-function-x-y)
    (check (valid-value-p object property))))

(defmethod test-dependant-property-inner ((o test-properties))
  "Verifies whether inheritance of properties behaves correctly."
  (let* ((object (make-instance 'test-properties-dependant-class-b))
         (get-property-named object 'objetive-class))
    (check (valid-value-p object property))
    (set-value-for-property object property 'entity-function-x-y)
    (check (valid-value-p object property))))
