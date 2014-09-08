
(defclass log-appender ()
  ())


(defmethod abstractp ((a (eql 'log-appender)))
  t)


(defclass file-appender (log-appender)
  ((file :initarg :file :initform "log.txt" :accessor file)))


(defmethod append-values ((a file-appender) &rest values)
  (with-open-file (ostream (format nil "~a.txt" (file a))
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
    (let ((*print-level* 64)
          (*print-length* 10000000))
      (dolist (i values)
        (format ostream "~a " i))
      (format ostream "~%")
      (stream:stream-flush-buffer ostream))))


(defclass print-appender ()
  ())


(defmethod append-values ((a print-appender) &rest values) 
  (let ((*print-level* 64)
        (*print-length* 10000000))
    (dolist (i values)
      (format nil "~a " i))
    (format nil "~%")))

(defclass msgbox-appender ()
  ())


(defclass logger ()
  ((appenders :initarg :appenders :accessor appenders)))


(defmacro log-debug (logger &rest args)
  `(dolist (i (appenders ,logger))
     (append-values i (get-log-time) :debug ,@args)))

(defmacro log-info (logger &rest args)
  `(dolist (i (appenders ,logger))
     (append-values i (get-log-time) :info ,@args)))

(defmacro log-error (logger &rest args)
  `(dolist (i (appenders ,logger))
     (append-values i (get-log-time) :error ,@args)))

(defun get-log-time ()
  (coerce (/ (get-internal-real-time) 1000) 'float))

(defun initialize-default-logger ()
  (setf *logger* (make-instance 'logger :appenders (list (make-instance 'file-appender :file (default-log-file-name)))))
  (log-info *logger* "Start init..."))

(defun initialize-gui-logger ()
  (appendf (appenders *logger*) (list (make-instance 'print-appender))))

(defun default-log-file-name ()
  (or (log-file-from-command) "d:\\temp\\log.txt"))

(defun log-file-from-command ()
  (let ((value (argument-from-key system:*line-arguments-list* ":log" 1)))
    (if value (read-from-string value))))