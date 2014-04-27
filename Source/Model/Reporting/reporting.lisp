
;; #TODO: Move to a new object / lib

(defun report-on-file (object path options)
  (report-on-file-path object path options))
  
(defun report-on-file-path (object path &rest options)
  (with-open-file (ostream path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (let ((reporter (make-instance 'text-stream-reporter :output-stream ostream :options (car options)))
          (*print-level* 64)
          (*print-length* 10000000))
      (report-on-text-stream reporter object))))

(defun report-properties (stream object &rest properties)
  (dolist (p properties)
    (report-property object p stream)))

(defun report-property (object property-name stream)
  (let ((property (property-named object property-name)))
    (format stream "~A: ~A~%" 
            (label property) 
            (get-value-for-property object property))))
