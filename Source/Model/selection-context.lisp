(defclass selection-context (object-with-properties)
  ((context :initform (make-hash-table) :accessor context)))


(defmethod add-to-context (object &rest args)
  "Adds to context the specification contained in args."
  (do ((x args (cddr x)))
      ((null x))
    (setf (gethash (car x) (context object))
          (cadr x))))

(defmethod get-from-context ((object selection-context) property-name)
  "Gets information for a property-name from o."
  (gethash property-name (context object)))
