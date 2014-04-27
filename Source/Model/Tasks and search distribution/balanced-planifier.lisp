(defclass balanced-planifier (equitative-planifier)
  ((connections :initarg :connections :initform nil :accessor connections)
   (task-assignments :initarg :task-assignments :initform nil :accessor task-assignments)))

