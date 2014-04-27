
(capi::define-interface interface-pane-intro-sequencer
    (interface-pane-editor-entity-base-opengl)
  ((owner :initarg :owner :accessor owner)
   (double-buffered-p :initarg :double-buffered-p :initform t :accessor double-buffered-p)
   (camera :initarg :camera :initform (make-camera) :accessor camera)
   (transform :initarg :transform :initform nil :accessor transform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform))
  (:panes 
   (simple-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 25 :selected-image 2
                                :help-key "Play"
                                :selection-callback 'menu-play-intro
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 24 :selected-image 1
                                :help-key "Stop"
                                :selection-callback 'menu-stop-intro
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 9 :selected-image 8
                                :help-key "Edit"
                                :selection-callback 'menu-edit-intro
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 4 :selected-image 9
                                :help-key "Load"
                                :selection-callback 'menu-load-intro
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 4 :selected-image 9
                                :help-key "Save"
                                :selection-callback 'menu-save-intro
                                :callback-type :interface-data)
                 (make-instance 'capi:toolbar-button :image 0 :selected-image 9
                                :help-key "FPS"
                                :selection-callback 'menu-show-fps
                                :callback-type :interface-data))
           :selection nil))
    :callback-type :interface-data
    :title "Actions"
    :title-position :frame
    :default-image-set (capi:make-general-image-set :id 'global-button-icons-images :image-count 6))
   (graphic capi:opengl-pane
               :configuration (list :rgba t :depth nil :double-buffered t)
               :display-callback 'redisplay-canvas
               :resize-callback 'resize-canvas
               :font (gp:make-font-description :family "Arial")
               :accessor graphic))
  (:layouts (main capi:column-layout '(simple-toolbar graphic)))
  (:default-initargs 
   :best-width 200 :best-height 200 
   :visible-min-width 70
   :visible-min-height 70
   :display-state :internal-borderless
   :destroy-callback 'destroy-interface))

(defmethod graphic-part ((i interface-pane-intro-sequencer))
  (graphic i))


(defclass pane-intro-sequencer (base-pane)
  ((model :initarg :model :initform nil :accessor model)))


(defmethod initialize-instance :after ((p pane-intro-sequencer) &rest args)
  (when (model p)
    (setf (drawing-context (model p)) (graphic (interface p)))))

(defmethod (setf model) (value (p pane-intro-sequencer))
  (setf (drawing-context value) (graphic (interface p))))

(defmethod interface-class ((p pane-intro-sequencer))
  "Answer <p> interface class."
  'interface-pane-intro-sequencer)

(defmethod displayed-object ((p pane-intro-sequencer))
  (model p))

(defmethod check-intro-time ((p pane-intro-sequencer))
  "Set <p> model time to current." 
  nil)

(defun menu-play-intro (interface data)
  "Play intro on <interface>."
  (let* ((pane (pane interface))
         (model (model pane)))
    (check-intro-time pane)
    (start-intro model)
    (setf (drawing-context model) (graphic interface))
    (draw-intro model)))

(defun menu-stop-intro (interface data)
  "Stop intro on <interface>."
  (let ((pane (pane interface)))
    (stop-intro (model pane))))

(defun menu-edit-intro (interface data)
  "Edit intro on <interface>."
  (let ((pane (pane interface)))
    (open-pane-intro-editor (model pane))))

(defun menu-load-intro (interface data)
  "Load intro on <interface>."
  (let ((pane (pane interface)))
    nil))

(defun menu-save-intro (interface data)
  "Save intro on <interface>."
  (let ((pane (pane interface)))
    nil))

(defun menu-show-fps (interface data)
  "Show fps on <interface>."
  (let ((pane (pane interface)))
    nil))


#|

;; #DEVELOPMENT: Variables
;;
;; *object*
;; *group-elements*
;; *element-index*
;; *element-level*
;;

(scene :camera camera :description description)
(text-object font)
(named-object 
(updater :position position :child position)
(text-updater :text text :object object)
(update-group group :update))
(hierarchy-group :elements elements :child child)
(element-from intro-object-list)
(element-list intro-object 1)

;; Scene 1
(scene 
 :camera :default-camera
 :description (child-list 
               (updater
                :position :center
                :child (text-updater 
                        :text '((0 . "") (2 . "Sample") (4 . ""))
                        :object (text-object "Arial")))
               (updater 
                :position :center 
                :child (named-object "example"))))
 
;; Scene 2
(scene 
 :camera :default-camera
 :description (updater 
               :position :center
               :child (text-updater 
                       :text '((0 . "") (2 . "Hi") (4 . ""))
                       :object (text-object "Arial"))))

;; Scene 3
(scene 
 :camera :default-camera
 :description 
 (hierarchy-group
  :update '(setf (x (pos *object*))
                 (* (cos (* (/ 360 *group-elements*) *element-index*))
                    (/ 40.0 *element-level*))
                 (y (pos *object*))
                 (* (sin (* (/ 360 *group-elements*) *element-index*))
                    (/ 40.0 *element-level*)))
  :elements (element-list 
             (sphere-object "texture1" 20.0))
  :child (hierarchy-group 
          :update nil
          :elements (element-list 
                     (sphere-object "texture1" 10.0)
                     (sphere-object "texture1" 10.0)
                     (sphere-object "texture1" 10.0)
                     (sphere-object "texture1" 10.0))
          :child (hierarchy-group 
                  :update nil
                  :elements (element-list 
                             (sphere-object "texture1" 10.0)
                             (sphere-object "texture1" 10.0)
                             (sphere-object "texture1" 10.0)
                             (sphere-object "texture1" 10.0))
                  :child nil))))

;; Scene 4
(scene 
 :camera :default-camera
 :description 
 (hierarchy-group
  :update '(setf (x (pos *object*))
                 (* (cos (* (/ 360 *group-elements*) *element-index*))
                    (/ 40.0 *element-level*))
                 (y (pos *object*))
                 (* (sin (* (/ 360 *group-elements*) *element-index*))
                    (/ 40.0 *element-level*)))
  :elements (element-from (sphere-object "texture1" 20.0) 1)
  :child (hierarchy-group 
          :update nil
          :elements (element-from (sphere-object "texture2" 10.0) 4)
          :child (hierarchy-group 
                  :update nil
                  :elements (element-from (sphere-object "texture3" 10.0) 4)
                  :child nil))))


;; Intro from objects composition
(let ((sequencer (make-instance 'intro-sequencer 
                                :definition '(make-intro-definition 
                                              :scene (make-scene 
                                                      :camera (get-ortho-2d-camera)
                                                      :description 
                                                      (make-elements-list
                                                       (make-updater 
                                                        :update ""
                                                        :child (make-elements-list
                                                                (make-text-object "Hi" "Arial" :texture "TEXTURE01" :update ""))))))
                                :draw-method (make-instance 'direct-render))))
  (make-instance 'pane-intro-sequencer :model sequencer))

(let ((sequencer (make-instance 'intro-sequencer 
                                :definition '(make-intro-definition 
                                              :scene (make-scene 
                                                      :camera (get-ortho-2d-camera)
                                                      :description 
                                                      (make-elements-list
                                                       (make-updater 
                                                        :update ""
                                                        :child (make-elements-list
                                                                (make-text-object "Hi" "Arial" :texture "TEXTURE01" :update ""))))))
                                :draw-method (make-instance 'direct-render))))
  (make-instance 'pane-intro-sequencer :model sequencer))


;; FIRST EXAMPLE 1 QUAD
(let ((sequencer (make-instance 'intro-sequencer 
                                :definition '(make-intro-definition 
                                              :scene (make-scene 
                                                      :camera (get-ortho-2d-camera)
                                                      :description 
                                                      (make-elements-list
                                                       (make-updater 
                                                        :update ""
                                                        :child (make-elements-list
                                                                (make-rectangle-object 2 :pos :float :texture "TEXTURE01" :update ""))))))
                                :draw-method (make-instance 'direct-render))))
  (make-instance 'pane-intro-sequencer :model sequencer))




(let ((sequencer (make-instance 'intro-sequencer 
                                :definition '(make-intro-definition 
                                              :scene (make-scene 
                                                      :camera (get-ortho-2d-camera)
                                                      :description 
                                                      (make-elements-list
                                                       (make-updater 
                                                        :update ""
                                                        :child (make-elements-list
                                                                (make-rectangle-object 2 :pos :float :color #(1.0 1.0 1.0)  :texture "TEXTURE01" :update "")
                                                                (make-rectangle-object 2 :pos :float :color #(1.0 1.0 1.0) :texture "TEXTURE01" :update "")
                                                                (make-rectangle-object 20 :pos #(33 20 0) :color #(1 1 0) :texture "" :update ""))))))
                                :draw-method (make-instance 'direct-render))))
  (make-instance 'pane-intro-sequencer :model sequencer))



(let ((sequencer (make-instance 'intro-sequencer 
                                :definition '(make-intro-definition 
                                              :scene (make-scene 
                                                      :camera (get-ortho-2d-camera)
                                                      :description 
                                                      (make-elements-from
                                                       (make-rectangle-object 20 :pos #(11 20 0) :color #(1 1 0) :texture "" :update "") 
                                                       10)))
                                :draw-method (make-instance 'direct-render))))
  (make-instance 'pane-intro-sequencer :model sequencer))




(make-instance 'random-selector :delta-time-min 5.0 :delta-time-max 5.0)


|#
