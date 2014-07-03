
(defclass tabbed-text-reporting-tool (object-with-properties)
  ((result-file :initarg :result-file :accessor result-file)
   (header-properties :initarg :header-properties :initform (default-reporting-headers) :accessor header-properties)
   (column-properties :initarg :column-properties :accessor column-properties)
   (graphics :initarg :graphics :accessor graphics)
   (items :initarg :items :accessor items)))


(defun default-reporting-headers ()
  '(objetive-class 
    task-planifier
    task-builder
    fitness-evaluator 
    algorithm 
    (algorithm selection-method)
    (fitness-evaluator samples) (fitness-evaluator fitness-function) (fitness-evaluator precision) (fitness-evaluator target-program)))


(defclass benchmark-reporting-tool (tabbed-text-reporting-tool)
  ((start-time :initarg :start-time :accessor start-time)
   (final-time :initarg :final-time :accessor final-time)))


(defmethod write-report ((o benchmark-reporting-tool) name)
  (with-open-file (ostream (format nil "~a.txt" (result-file o))
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (let ((*print-level* 64)
          (*print-length* 10000000))
      (setf (start-time o) (get-universal-time))
      (format ostream "~%<HEADER>~%~%" name)
      (let ((item (first (items o))))
        (dolist (i (header-properties o))
          (format ostream (concatenate 'string "~a: ~a~%") 
                  (string-downcase (format nil "~a" i))
                  (get-value-for-property-named item i))))
      (format ostream (space-string o))
      (format ostream "~%RESULTS~%" name)
      (format ostream (name-format o nil) "Task")
      (format ostream (space-string o))
      (dolist (i (column-properties o))
        (format ostream (concatenate 'string "~" (format nil "~a" (max (value-column-length o) (length (format nil "~a" i)))) ",a") i)
        (format ostream (space-string o)))
      (format ostream "~%~%")
      (stream:stream-flush-buffer ostream)
      (dolist (i (items o))
        (let ((benchmark (default-benchmark i)))
          (process-item o i benchmark)
          (report-item o i ostream benchmark)
          (stream:stream-flush-buffer ostream)))
      (setf (final-time o) (get-universal-time))
      (format ostream "~%<FOOTER>~%~%")
      (format ostream "Start time: ~a~%" (date-to-string (start-time o)))
      (format ostream "End time: ~a~%" (date-to-string (final-time o)))
      (format ostream "Total: ~a seconds~%" (- (final-time o) (start-time o)))
      (format ostream "~%<GRAPHICS>~%")
      (stream:stream-flush-buffer ostream))))

(defmethod report-item ((o benchmark-reporting-tool) item ostream benchmark)
  (let ((name-string (format nil "~a" (name item))))
    (setf (current-item benchmark) item)
    (format ostream (name-format o item) (subseq name-string 0 (min (length name-string) (name-column-length o))))
    (format ostream (space-string o))
    (dolist (i (column-properties o))
      (let* ((property-value (get-value-for-property-named benchmark i))
             (value (if (functionp property-value) (funcall property-value item) property-value)))
        (format ostream (format 
                         nil 
                         (concatenate 'string "~" (format nil "~a" (max (value-column-length o) (length (format nil "~a" i)))) ",a")
                         (format nil (column-format o item) value)))
        (format ostream (space-string o))))
    (format ostream "~%")))

(defmethod default-benchmark (item)
  (make-instance 'task-benchmark))

(defmethod column-format ((o benchmark-reporting-tool) column)
  (concatenate 'string "~" (format nil "~a" (value-column-length o)) "," (format nil "~a" (value-column-decimals o)) "F"))

(defmethod name-format ((o benchmark-reporting-tool) column)
  (concatenate 'string "~" (format nil "~a" (name-column-length o)) ",a"))

(defmethod name-column-length ((o benchmark-reporting-tool))
  30)

(defmethod value-column-length ((o benchmark-reporting-tool))
  8)

(defmethod value-column-decimals ((o benchmark-reporting-tool))
  2)

(defmethod process-item ((o benchmark-reporting-tool) item benchmark)
  (prepare-benchmark benchmark item)
  (execute-until-finished item "Reporting task execution..."))

(defmethod space-string ((o benchmark-reporting-tool))
  "  ")


(defclass test-benchmark-reporting-tool (test-case)
  ())


(defmethod test-reporting-from-file ((o test-benchmark-reporting-tool))  
  (let* ((a (make-instance 'search-task))
         (b (make-instance 'search-task))
         (reporter (make-instance 'benchmark-reporting-tool
                                  :column-properties '(best-fitness likelihood-of-optimality average-fitness-value likelihood-of-evolution-leap time)
                                  :items (list a b)
                                  :result-file "d:\\temp\\result")))
    (setf (max-generations (algorithm a)) 5
          (max-generations (algorithm b)) 5)
    (write-report reporter "name")))

(defun tasks-from-directory (path)
  (let* ((spec (format nil "~A\\*.task" path))
         (files (directory spec :directories nil)))
    (mapcar (lambda (object) (eval (read-from-string (car (load-from-file object)))))
            files)))

(defun date-to-string (date)
  (multiple-value-bind (s m h)
      (decode-universal-time date)
    (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))
