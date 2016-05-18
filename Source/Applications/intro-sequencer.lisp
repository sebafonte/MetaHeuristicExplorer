;; Object grouping & ordering for rendering
(defclass object-group ()
  ())


;; Intro definition class
(defclass intro-definition (object-with-properties)
  ((scenes :initarg :scenes :accessor scenes)
   (scene-selector :initarg :scene-selector :accessor scene-selector)
   (theme :initarg :theme :accessor theme)))


;; Intro sequencer or player
(defclass intro-sequencer (object-with-properties)
  ((definition :initarg :definition :accessor definition)
   (intro-state :initarg :intro-state :accessor intro-state)
   (objects :initarg :objects :accessor objects)
   (current-time :initarg :current-time :initform 0 :accessor current-time)
   (draw-method :initarg :draw-method :accessor draw-method)
   (drawing-context :initarg :drawing-context :initform nil :accessor drawing-context)))


(defmethod initialize-instance :after ((o intro-sequencer) &rest args)
  "Initialize <o>."
  (when (definition o)
    (setf (intro-state o) (intro-from-description o (definition o))))
  (setf (objects o) 
        (all-objects (mapcar 
                      (lambda (o) (description o))
                      (scenes (intro-state o))))))

(defun all-objects (object)
  "Answer all intro objects from <scene> description."
  (unique-eql (select (flatten object) 
                      (lambda (o) (is-kind-of o 'intro-object)))))

(defun intro-from-description (object description)
  "Answer description tree for <description>."
  (setf (intro-state object)
        (eval (deparse 
         (parse (grammar (language object)) description)))))

(defmethod language ((a intro-sequencer))
  *intro-sequencer-language*)

(defmethod start-intro ((o intro-sequencer))
  (setf (current-time o) 0)
  (start-theme o))

(defmethod stop-intro ((o intro-sequencer))
  (stop-theme o))

;; #TODO: Start MIDI / MP3 music playback
(defmethod start-theme ((o intro-sequencer))
  nil)

(defmethod stop-theme ((o intro-sequencer))
  nil)

(defmethod update ((o intro-sequencer))
  (process-time o)
  (update-objects o))

(defmethod process-time ((o intro-sequencer))
  (incf (current-time o) (time-delta o)))

(defmethod update-objects ((o intro-sequencer))
  "Update all <o> objects."
  (dolist (i (objects o))
    (update-object o i)))

;; #TODO: Change with a real time delta
(defmethod time-delta ((o intro-sequencer))
  0.01)

;; #HARDCODE temporal
(defmethod current-intro-scene ((o intro-sequencer))
  (if (scene-selector (intro-state o))
      (current-scene (scene-selector (intro-state o)) (scenes (intro-state o)))
    (first (scenes (intro-state o)))))

(defmethod draw-opengl-on ((o intro-sequencer) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (let* ((width (slot-value canvas 'graphics-ports::width))
         (height (slot-value canvas 'graphics-ports::height))
         (ix (/ width 10))
         (iy (/ height 10))
         (cx (min width 100))
         (cy (min height 100))
         (dx (/ width cx))
         (dy (/ height cy)))
    (update o)
    (opengl:rendering-on (canvas)
      (opengl:gl-clear opengl:*GL-ALL-ATTRIB-BITS*)
      (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
      (draw-intro o))
  (handler-case (opengl:swap-buffers canvas)
    (error (function) nil))))

(defmethod draw-intro ((o intro-sequencer))
  (let ((scene (current-intro-scene o))
        (canvas (drawing-context o)))
    (render-camera scene)
    (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
    (draw-objects (draw-method o) (objects scene))))


;; Base objects
(defclass intro-object (object-with-properties)
  ((name :initarg :name :accessor name)))


(defmethod update-object ((o intro-sequencer) (o intro-object))
  nil)

(defmethod register-object ((o intro-object) dictionary)
  (appendf (gethash :objects dictionary) (list o)))


(defclass mutable-intro-object (intro-object)
  ((rules :initarg :rules :accessor rules)))


(defmethod programs ((o mutable-intro-object))
  (error "Implemented by subclass"))

(defmethod rules ((o mutable-intro-object))
  (error "Implemented by subclass"))


(defclass explorer-object-wrapper (mutable-intro-object)
  ((object :initform :object :accessor object)))

;; #TODO: Change for #expressions, which should include program and language
(defmethod programs ((o mutable-intro-object))
  "Answer a list of expressions for <o>."
  (list object))

(defmethod rules ((o mutable-intro-object))
  "Answer desired rules to be satisfied by <o>."
  nil)

;; Scene objects
(defclass scene (intro-object)
  ((camera :initarg :camera :accessor camera)
   (description :initarg :description :initform nil :accessor description)
   (objects :initarg :objects :initform nil :accessor objects)))

(defmethod initialize-instance :after ((o scene) &rest args)
  (when (description o)
    (set-objects-list o)))

(defmethod (setf description) (value (o scene))
  (setf (slot-value o 'description) value)
  (set-objects-list o))

(defmethod render-camera ((o scene))
  (draw-object (camera o)))

(defmethod draw-object ((o scene))
  (progn nil))

(defmethod set-objects-list ((o scene))
  (let ((dictionary (make-hash-table)))
    (register-object (camera o) dictionary)
    (dolist (i (description o))
      (register-object i dictionary))
    (setf (objects o) (gethash :objects dictionary))))

(defclass scene-object (intro-object)
  ((pos :initarg :pos :initform #(0.0 0.0 0.0) :accessor pos)
   (texture :initarg :texture :initform nil :accessor texture)
   (color :initarg :color :initform #(1.0 1.0 1.0) :accessor color)))

(defmethod set-object-position ((o scene-object))
  (when (arrayp (pos o))
      (opengl:gl-translatef (safe-float-coerce (aref (pos o) 0)) 
                            (safe-float-coerce (aref (pos o) 1)) 
                            (safe-float-coerce (aref (pos o) 2)))))


(defmethod set-object-texture ((o scene-object))
  (when (arrayp (color o))
    (opengl:gl-color3-f (safe-float-coerce (aref (pos o) 0))
                        (safe-float-coerce (aref (pos o) 1)) 
                        (safe-float-coerce (aref (pos o) 2)))))

;; Scene objects (graphic in fact)
(defclass image-object (scene-object)
  ((alpha :initarg :alpha :accessor alpha)
   (size :initarg :size :accessor size)))

(defmethod draw-object ((o image-object))
  (error "Not implemented yet"))

(defclass text-object (scene-object)
  ((font :initarg :font :accessor font)
   (text :initarg :text :accessor text)
   (texture :initarg :texture :accessor texture)))

(defmethod draw-object ((o text-object))
  (error "Not implemented yet"))

(defclass hierarchy-group-object (scene-object)
  ((child :initarg :child :accessor child)
   (elements :initarg :elements :accessor elements)
   (update :initarg :update :accessor update)))

(defmethod register-object :after ((o hierarchy-group-object) dictionary)
  (dolist (i (elements o))
    (register-object i dictionary))
  (dolist (i (child o))
    (register-object i dictionary)))

(defclass geometry-object (scene-object)
  ())

(defmethod abstractp ((o (eql 'geometry-object)))
  "Answer whether <o> is abstract."
  t)

(defmethod draw-object ((o geometry-object))
  (error "Subclass responsibility."))

(defclass point-object (geometry-object)
  ((size :initarg :size :accessor size)))

(defmethod draw-object ((o point-object))
  (opengl:gl-begin opengl:*gl-points*)
  ;; #TODO: Check context and use 2 / 3 dimensions
  ;;        Check best approach
  ;(gl-app-vertex-3d (x-component o) (y-component o))
  (gl-app-vertex-3d (x-component o) (y-component o) (z-component o))
  (opengl:gl-end))

(defclass cube-object (geometry-object)
  ((side :initarg :side :accessor side)))

(defmethod draw-object ((o cube-object))
  nil)

(defclass rectangle-object (geometry-object)
  ((side :initarg :side :accessor side)))

(defmethod draw-object ((o rectangle-object))
  (let ((side (side o)))
    (set-object-position o)
    (set-object-texture o)
    ;; Draw
    (opengl:gl-begin opengl:*gl-quads*)
    (opengl:gl-vertex2-f 0.0 0.0)
    (opengl:gl-vertex2-f 0.0 (safe-float-coerce side))
    (opengl:gl-vertex2-f (safe-float-coerce side) (safe-float-coerce side))
    (opengl:gl-vertex2-f (safe-float-coerce side) 0.0)
    (opengl:gl-end)))

(defclass sphere-object (geometry-object)
  ((radius :initarg :radius :accessor radius)))

(defmethod draw-object ((o sphere-object))
  nil)

(defclass mesh-object (geometry-object)
  ((points :initarg :points :accessor points)
   (normals :initarg :normals :accessor normals)
   (texture :initarg :texture :accessor texture)
   (texture-coords :initarg :texture-coords :accessor texture-coords)
   (faces :initarg :faces :accessor faces)))

(defmethod draw-object ((o mesh-object))
  (error "Not implemented yet"))

(defclass light-object (scene-object)
  ())

;; #TODO: Consider to add to a container, be ordered, etc.
(defmethod draw-object ((o light-object))
  nil)

(defclass camera-object (scene-object)
  ((from :initarg :from :accessor from)
   (to :initarg :to :accessor to)))

(defmethod draw-object ((o camera-object))
  (error "Not implemented yet"))

(defmethod register-object ((o camera-object) dictionary)
  (appendf (gethash :camera dictionary) (list o)))

(defclass ortho-2d-camera-object (scene-object)
  ((value-a :initarg :value-a :initform 0.d0 :accessor value-a)
   (value-b :initarg :value-b :initform 100 :accessor value-b)
   (value-c :initarg :value-c :initform 100 :accessor value-c)
   (value-d :initarg :value-d :initform 0.d0 :accessor value-d)))

(defmethod draw-object ((o ortho-2d-camera-object))
  (initialize-ortho-2d (value-a o) (value-b o) (value-c o) (value-d o)))

(defmethod register-object ((o ortho-2d-camera-object) dictionary)
  (appendf (gethash :camera dictionary) (list o)))

;; Updater objects
(defclass object-updater (intro-object)
  ((child :initarg :child :accessor child)
   (expression :initarg :expression :accessor expression)))

(defmethod register-object :after ((o object-updater) dictionary)
  (dolist (i (child o))
    (register-object i dictionary)))

(defmethod register-object ((o object-updater) dictionary)
  (appendf (gethash :updaters dictionary) (list o)))


(defmethod update-object ((o intro-sequencer) (o object-updater))
  nil)

(defclass geometry-updater (object-updater)
  ())

(defmethod update-object ((o intro-sequencer) (o geometry-updater))
  nil)

(defclass object-group-updater (object-updater)
  ((mode :initarg :mode :accessor mode)))

(defmethod update-object ((o intro-sequencer) (o object-group-updater))
  nil)


;; Scene selectors
(defclass scene-selector (intro-object)
  ())

(defmethod current-scene ((o scene-selector) scenes)
  (error "Subclass responsibility."))

(defmethod abstractp ((o (eql 'scene-selector)))
  "Answer whether <o> is abstract."
  t)


(defclass frame-list-selector (scene-selector)
  ())


(defmethod current-scene ((o frame-list-selector) scenes)
  (nth 0 scenes))


(defclass random-selector (scene-selector)
  ((current-frame :initarg :current-frame :accessor current-frame)
   (next-frame :initarg :next-frame :accessor next-frame)
   (delta-time-min :initarg :delta-time-min :accessor delta-time-min)
   (delta-time-max :initarg :delta-time-max :accessor delta-time-max)))

(defmethod current-scene ((o random-selector) scenes)
  (nth 0 scenes))


;; Objects drawing methods
(defclass draw-method (intro-object)
  ())

(defmethod draw-objects ((o draw-method) objects)
  nil)


(defclass direct-render (draw-method)
  ())

(defmethod draw-objects ((o direct-render) objects)
  (dolist (i objects)
    (draw-object i)))


(defclass group-render (draw-method)
  ((classifier :initarg :classifier :accessor classifier)))

(defmethod draw-objects ((o group-render) objects)
  (let ((group (build-group (classifier o) objects)))
    (draw-group o group)))

(defmethod draw-group ((o group-render) group)
  nil)


;; Object classifier for build the groups used by group rendering functions for optimization purposes
(defclass classifier (intro-object)
  ())


(defmethod build-group ((o classifier) objects)
  "Answer a tree representation of a render group."
  objects)


(defclass texture-classifier (intro-object)
  ())


(defmethod build-groups ((o texture-classifier))
  "Answer a tree representation of a render group."
  (error "Not implemented yet"))


(defclass intro-builder (object-with-properties)
  ((path :initarg :path :accessor path)))


;; #PACKAGE take 'intro posfix
(defmethod build-intro ((o intro-object))
  nil)


