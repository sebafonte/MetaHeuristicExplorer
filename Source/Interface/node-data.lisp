(defclass node-data (base-model)
  ((node :initarg :node :initform nil :accessor node)
   (index :initarg :index :initform nil :accessor index)
   (object :initarg :object :initform nil :accessor object)))


(defmethod to-string ((o node-data))
  "Answer <o> string representation."
  (let ((node (node o)))
    (string (format nil (if (keywordp node) ":~A" "~D") node))))

;; #TODO: Make from this two functions one, take care with multiprocessing
(let ((*index*))
  (defun genotype-nodes-data (object)
    "Answer a node-data list with <object> nodes representation."
    (setf *index* 0)
    (if (object object)
        (genotype-nodes-data-recursive object (if object (program object) ""))
      nil))
  (defun genotype-nodes-data-recursive (object x)
    "Answer a node-data list with <object> nodes representation."
    (incf *index*)
    (if x 
        (if (consp x)
            (cons (genotype-nodes-data-recursive object (car x)) 
                  (genotype-nodes-data-recursive object (cdr x)))
          (make-instance 'node-data :object object :node x :index *index*)))))
