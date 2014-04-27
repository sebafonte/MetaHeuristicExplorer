
;;; New children
(defmethod create-new-children ((o entity) algorithm operation parents) 
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <hijo> geneotype."
  (let ((child (operate operation (language algorithm) parents)))
    (prepare-children-from o child algorithm)))

(defmethod create-new-children ((o entity-function) algorithm operation parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <o> geneotype."
  (let* ((program-list (mapcar (lambda (i) (program i)) parents))
         (new-expression (operate operation (language algorithm) program-list)))
    (prepare-children-from o new-expression algorithm)))

(defmethod create-new-children ((o search-task) algorithm operation parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <o> geneotype."
  (let* ((program-list (mapcar (lambda (i) (program i)) parents))
         (new-expression (operate operation (language algorithm) program-list)))
    (prepare-children-from o new-expression algorithm)))

(defmethod create-new-children ((o entity-linear-ordering) algorithm operation parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <hijo> geneotype."
  (let ((child (operate operation algorithm parents)))
    (prepare-children-from o child algorithm)))

;;; New VALID children
(defmethod create-new-valid-children ((o entity) algorithm parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <hijo> geneotype."
  (let ((children (create-valid-expression o (language algorithm) parents)))
    (prepare-children-from o children algorithm)))

(defmethod create-new-valid-children ((o entity-function) algorithm parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <hijo> geneotype."
  (let* ((parent-programs (mapcar (lambda (i) (program i)) parents))
         (children (create-valid-expression o (language algorithm) parents)))
    (prepare-children-from o children algorithm)))

(defmethod create-new-valid-children ((o search-task) algorithm parents)
  "Perform <operation> to <parents>, perform final corrections and simplifications.
   The result replaces <o> geneotype."
  (let* ((program-list (mapcar (lambda (i) (program i)) parents))
         (children (create-valid-expression o (language algorithm) program-list)))
    (prepare-children-from o children algorithm)))

;;; Other
(defmethod create-valid-expression ((o entity) language parents)
  (apply (valid-new-expresion-function language) 
         (list language parents)))
