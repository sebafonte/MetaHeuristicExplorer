;;; At the moment these are just some function and classes. The idea is to create a nice class hierarchy for menus


;; #TODO: Example from CAPI, refactor
(capi:define-interface interface-popup-menu ()
  ((keyword :initarg :right-click-selection-behavior))
  (:panes
   (list-panel capi:list-panel
               :items '("test")
               :visible-min-height '(:character 4)
               :pane-menu 'my-menu
               :interaction :multiple-selection
               :right-click-selection-behavior keyword)))


(defun my-menu (pane data x y)
  (declare (ignore pane x y))
  (make-instance 'capi:menu
                 :items (list "Hi There"
                              ""
                              "Here's the data:"
                              data)))

(defclass pane-menu ()
  ((owner :initarg :owner :initform nil :accessor owner)
   (description :initarg :description :accessor description)))

(defmethod capi-menu-description ((p pane-menu))
  "Answer a list with capi description for a pane-menu instance."
  nil)



#|
(defun popup-test-menu (pinboard x y &optional gspec)
  (capi:display-popup-menu
   (make-instance 'capi:menu :items '(1 2 3))
   :owner pinboard :x x :y y))

(capi:contain
 (make-instance 'capi:pinboard-layout
                :input-model
                '((:post-menu popup-test-menu))
                :visible-min-width 100
                :visible-min-height 100))

(capi:display
 (make-instance 'interface-popup-menu
                :right-click-selection-behavior
                :clicked/restore/restore))
|#