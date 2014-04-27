(defclass test-generational-algorithm (test-core-objects) ())


(defmethod test-execution ((o test-core-objects))
  "Execute a test generation algorithm and check results."
  (let ((algorithm (default-algorithm o)))
    (buscar algorithm)))

