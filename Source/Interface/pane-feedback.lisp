
(defclass pane-feedback (base-pane)
  ((registry :initarg :registry :initform nil :accessor registry)
   ;; Function that says the individual is acceptable
   (accept-function :initarg :accept-function :initform #'accept-function-default :accessor accept-function)
   ;; Object used to determine the fitness of an individual and the registry
   ;; #TODO: 
   (fitness-assignment :initarg :fitness-assignment :initform 'fitness-assignment-distance :accessor fitness-assignment)))


(defmethod interface-class ((p pane-feedback))
  "Answer <p> interface class."
  'interface-pane-feedback)

;; #TODO: Now registering controls are buttons, but should use drag and drop only
(capi:define-interface interface-pane-feedback (base-interface)
  ()
  (:panes
   (label-operations capi:title-pane :text "Operations" :visible-min-width 120)
   (button-trash capi:push-button :text "Trash" :visible-min-width 120 :visible-min-height 50)
   (button-method capi:push-button :text "Register method" :visible-min-width 120 :visible-min-height 50)
   (button-good capi:push-button :text "Good" :visible-min-width 120 :visible-min-height 50)
   (button-bad capi:push-button :text "Bad" :visible-min-width 120 :visible-min-height 50)
   (slider-fitness capi:slider :orientation :horizontal :visible-min-width 100 :visible-min-height 20))
  (:layouts 
   (main-layout capi:column-layout '(layout-register slider-fitness))
   (layout-register capi:row-layout '(button-trash button-method button-good button-bad)))
  (:default-initargs :best-width 100 :best-height 75 :title "Retroalimentation"))

(defmethod estimated-fitness ((p pane-feedback) object)
  "Answer <object> estimated fitness for <p>."
  1)

(defmethod default-register-method ((p pane-feedback))
  "Answer default register method for <p>."
  nil)

(defmethod register-estimation ((p pane-feedback) object estimated-fitness)
  "Register <object> into <p> registery with <estimated-fitness>."
  nil)

(defmethod clear ((p pane-feedback))
  "Clear <p> registry. "
  (setf (registry p) nil))
