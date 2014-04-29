
(defclass base-pane (base-model)
  ((name :initarg :name :initform "" :accessor name)
   (title :initarg :title :initform "" :accessor title)
   (interface :initarg :interface :initform nil :accessor interface)
   (interface-arguments :initarg :interface-arguments :initform nil :accessor interface-arguments)))


(defclass base-interface (capi:interface)
  ((pane :initarg :pane :initform nil :accessor pane)))


(defmethod (setf pane) ((interface base-interface) (pane base-pane))
  "Set <pane> to <interface> pane."
  (setf (slot-value interface 'pane) pane))

(defmethod initialize-instance :after ((p base-pane) &optional &key mdi-interface (open t) (prepare t))  
  "Initialize <p>."
  (if prepare 
      (initialize-interface p))
  (if open
      (if mdi-interface
          (open-pane p :mdi-interface mdi-interface)
        (open-pane p))))

(defmethod initialize-interface ((p base-pane))
  (setf (interface p) (create-interface p)
        (pane (interface p)) p))

(defmethod open-pane ((p base-pane) &key mdi-interface)
  "Display <p> on <mdi-interface>."
  (display-interface p mdi-interface)
  (post-initialize-interface p))

(defmethod display-interface ((p base-pane) mdi-interface)
  "Display <p> interface on <mdi-interface>."
  (if mdi-interface 
      (capi:display (interface p) :screen mdi-interface)
    (capi:display (interface p))))

(defmethod create-interface ((p base-pane))
  "Create <p> interface."
  (apply 'make-instance
         (append (list (interface-class p))
                 (interface-argument-list p))))

(defmethod interface-argument-list ((p base-pane))
  "Answer <p> interface argument list."
  (interface-arguments p))

(defmethod create-population-editors ((p base-pane) population)
  "Answer a list of editors for individuals in <population>."
  (let ((count (length (individuals-array population)))
        (result))
    (dotimes (i count i)
      (push (make-image-editor-pane
             :model (aref (individuals-array population) i)
             :mdi-interface (interface p)
             :open nil) 
            result))
    result))

(defmethod execute-menu-action ((p base-pane) actions interface data)
  "Execute menu selected action on <p>."
  (let ((action (cadr (assoc data actions :test #'equalp))))
    (if action (funcall action interface data))))

(defun multi-column-list-panel-test-column-items (state)
  (loop for (nil data) on state by 'cddr collect data))

(defmethod element-interface ((i base-interface))
  i)

(defmethod element-interface ((i t))
  (capi:element-interface i))

(defmethod destroy-interface :after ((interface base-interface))
  "Perform actions when <interface> is destroyed."
  (clear-events-for (pane interface))
  (clear-events-for interface))

(defmethod post-initialize-interface ((p base-pane))
  "Post initialize actions for <p>."
  nil)