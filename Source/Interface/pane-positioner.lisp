
(defclass pane-positioner (base-model)
  ())


(defmethod set-position ((object pane-positioner) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (error "Subclass responsibility"))

(defmethod abstractp ((o (eql 'pane-positioner)))
  "Answer whether <o> is abstract."
  t)


(defclass any-position (pane-positioner)
  ())

(defmethod set-position ((object any-position) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (progn nil))

(defmethod set-initial-position ((object any-position) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (progn nil))

(defclass near-parent-position (pane-positioner)
  ((parent-interface :initarg :parent-interface :accessor parent-interface)
   (delta-x :initarg :delta-x :initform 20 :accessor delta-x)
   (delta-y :initarg :delta-y :initform 0 :accessor delta-y)))

(defmethod set-position ((object near-parent-position) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (progn nil))

(defmethod set-initial-position ((object near-parent-position) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (appendf (interface-arguments (pane interface)) (list :x 0 :y 0)))

;; #TODO: 
(defclass free-main-parent-position (pane-positioner)
  ())

  
(defmethod set-position ((object free-main-parent-position) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (error "Not implemented yet"))


(defclass keep-last-position (pane-positioner)
  ())

  
(defmethod set-position ((object keep-last-position) interface)
  "Set <interface> position depending on <pane-interface> and <object> strategy."
  (error "Not implemented yet"))



(defun default-pane-positioners ()
  (list (system-get 'any-position)
        (system-get 'free-main-parent-position)
        (system-get 'near-parent-position-x)
        (system-get 'near-parent-position-y)
        (system-get 'keep-last-position)))

(defun initialize-default-pane-positioners ()
  (system-add-with-name (make-instance 'any-position) 'any-position)
  (system-add-with-name (make-instance 'free-main-parent-position) 'free-main-parent-position)
  (system-add-with-name (make-instance 'near-parent-position :delta-x 20 :delta-y 0) 'near-parent-position-x)
  (system-add-with-name (make-instance 'near-parent-position :delta-x 0 :delta-y 20) 'near-parent-position-y)
  (system-add-with-name (make-instance 'keep-last-position) 'keep-last-position))

