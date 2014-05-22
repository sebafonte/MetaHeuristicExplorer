
(defclass balanced-planifier (equitative-planifier)
  ((connections :initarg :connections :initform nil :accessor connections)
   (task-assignments :initarg :task-assignments :initform nil :accessor task-assignments)))


(defmethod connections-by-load ((p balanced-planifier))
  (sort (active-connections (connection-administrator p)) 
        'connection-weighted-load-comparator))

(defun connection-weighted-load-comparator (a b)
  (> (connection-load a)
     (connection-load b)))