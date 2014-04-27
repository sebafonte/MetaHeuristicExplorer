

(capi:define-interface redrawing-with-opengl ()
  ((owner :initarg :owner :initform nil :accessor owner)
   (camera :initform (make-camera) :initarg :camera :accessor camera)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (background :initform (make-default-background) :initarg :background :accessor background)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy))
  (:panes
   (opengl-pane capi:opengl-pane 
                :accessor opengl-pane
                :configuration (list :rgba t :depth nil :double-buffered t)
                :min-width 400
                :min-height 400
                :max-width 400
                :max-height 400
                :display-callback 'redisplay-canvas
                :resize-callback 'resize-canvas
                :input-model '(((:button-1 :press) viewer-button-1)
                               ((:button-2 :press) viewer-button-2)
                               ((:button-3 :press) viewer-button-2)
                               ((:button-1 :shift :press) viewer-button-1-shift)
                               ((:motion :button-1) viewer-motion-button-1)
                               ((:motion :button-2) viewer-motion-button-2)
                               ((:motion :button-3) viewer-motion-button-2)
                               ((:motion :button-1 :shift) viewer-motion-button-1-shift))
                :reader canvas
                :font (gp:make-font-description :family "Arial")))
  (:layouts (opengl-layout capi:pinboard-layout '((opengl-pane))
                           :pane-menu (lambda (pane object x y)
                                        (make-pane-menu-with-submenus 
                                         pane object x y 
                                         (options-menu-description pane object)))
                           :input-model `(((:button-1 :press)
                                           ,#'(lambda (pane x y)
                                                (drag-example-drag-from pane x y)))
                                          ((:button-1 :release)
                                           ,#'(lambda (pane x y)
                                                (drop-example-drop-string-callback pane x y))))
                           :accessor opengl-layout)))


;;; DISPLAY FUNCTIONS
(defun redisplay-canvas (canvas &rest ignore)
  (declare (ignore ignore))
  (with-slots ((viewer capi:interface)) canvas
    (unless (icotransform viewer)
      (initialize-viewer viewer))
    (opengl:rendering-on (canvas)
      (draw (camera viewer))
      (opengl:with-matrix-pushed
        (opengl:gl-mult-matrixd (light-transform viewer))
        (opengl:gl-light-modelfv opengl:*gl-light-model-ambient* *light-model-ambient*)
        (opengl:gl-light-modelf opengl:*gl-light-model-local-viewer* 0.0)
        (opengl:gl-light-modelf opengl:*gl-light-model-two-side* 0.0)
        (opengl:gl-enable opengl:*gl-light0*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-position* *light-position*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-ambient* *light-ambient*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-diffuse* *light-diffuse*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-specular* *light-specular*))
      (opengl:with-matrix-pushed
        (opengl:gl-mult-matrixd (icotransform viewer))
        (opengl:gl-cull-face opengl:*gl-back*)
        (opengl:gl-enable opengl:*gl-cull-face*)
        (opengl:gl-enable opengl:*gl-color-material*)
        (opengl:gl-color-material opengl:*gl-front* opengl:*gl-ambient-and-diffuse*)
        (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-specular* *material-specular*)
        (opengl:gl-materialf opengl:*gl-front* opengl:*gl-shininess* *material-shininess*)
        (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-emission* *material-emission*)
        ;(draw (icosahedron viewer))
        )
      (handler-case (opengl:swap-buffers canvas)
		(error (function) nil)))))

(defun resize-canvas (canvas x y width height)
  x y
  (when #+Win32 (win32:is-window-visible (win32:pane-hwnd (capi-internals:representation canvas)))
	#-Win32 T
    (opengl:rendering-on (canvas)
      (opengl:gl-viewport 0 0 width height))
    (with-slots ((viewer capi:interface)) canvas
      (setf (aspect (projection (camera viewer)))
            (coerce (/ width height) 'double-float)))
    (redisplay-canvas canvas)))


;;; VIEW INITIALIZATION
(defun initialize-viewer (icosahedron-viewer)
  (opengl:rendering-on ((canvas icosahedron-viewer))
    (setf (icotransform icosahedron-viewer) (make-gl-double-vector 16))
    (setf (light-transform icosahedron-viewer) (make-gl-double-vector 16))
    (initialize-transform (icotransform icosahedron-viewer))
    (initialize-transform (light-transform icosahedron-viewer))
    (reset-lights-and-materials)))

;; #TMP
(defun reset-lights-and-materials ()
  (setf *light-model-ambient* (gl-single-vector 0.0 0.0 0.0 1.0))
  (setf *light-position* (gl-single-vector 1.0 2.0 3.0 1.0))
  (setf *light-ambient* (gl-single-vector 0.1 0.1 0.1 1.0))
  (setf *light-diffuse* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *light-specular* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *material-specular* (gl-single-vector 0.1 0.0 0.0 1.0))
  (setf *material-shininess* 64.0)
  (setf *material-emission* (gl-single-vector 0.0 0.0 0.0 1.0)))

(defun initialize-transform (transform)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun make-default-background ()
  (gl-single-vector 0.5 0.5 0.5 1.0))


;;; BUTTON ACTIONS
(defun viewer-button-1 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-button-2 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-motion-button-1 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-icosahedron viewer (- x (car last)) (- y (cdr last))))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defun viewer-motion-button-2 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-light viewer (- x (car last)) (- y (cdr last))))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defun viewer-button-1-shift (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-motion-button-1-shift (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (let ((eye (eye (camera viewer))))
          (setf (xyz-y eye)
                (max (+ (xyz-y eye) (/ (- (car last) x) 20)) 2d0)))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))


;;; ------------------------------
;;; Vertex can be pass through to 'C'
;;; vertexes list of gl-vertexes (not passed to 'C'
;;; ------------------------------

(declaim (inline gl-vertex gl-vertexes))
(defun gl-vertex (x y z w)
  (gl-double-vector x y z w))

(defun gl-vertexes (contents)
  (mapcar #'(lambda (c) (apply 'gl-double-vector c)) contents))


;;; ------------------------------
;;; XYZ coordinate
;;; ------------------------------

(defstruct xyz 
  (x  0.0d0 :type double-float)
  (y  0.0d0 :type double-float)
  (z  0.0d0 :type double-float))


;;; ------------------------------------------------------------
;;; Class object
;;; 
;;; This superclass just manages display lists for the subclasses.

(defclass object ()
  ((use-display-list :initform nil :initarg :use-display-list :accessor use-display-list)
   (display-list :initform nil :initarg :display-list :accessor display-list)
   (extra-display-lists :initform nil :accessor extra-display-lists)
   (viewer :accessor viewer)))

(defmethod draw :around ((object object))
  (if (use-display-list object)
      (if (display-list object)
          (progn
            (opengl:gl-call-list (display-list object))
            (let ((draw (sys:cdr-assoc :draw (extra-display-lists object))))
              (mapc 'opengl:gl-call-list draw)))
        (progn
          (set-up-gl-fonts (canvas (viewer object)) object)
          (let ((n (opengl:gl-gen-lists 1)))
	    (if (plusp n)
                (progn
	          (opengl:gl-new-list n opengl:*gl-compile-and-execute*)
                  (call-next-method)
	          (opengl:gl-end-list)
	          (setf (display-list object) n))
	      (progn 
	        (format t "~%~s:No more display list indexes!" object)
                (call-next-method))))))
    (call-next-method)))

(defmethod (setf use-display-list) :after (value (object object))
  (unless value
    (delete-display-list object)))

(defmethod delete-display-list ((object object))
  (when (display-list object)
    (opengl:gl-delete-lists (display-list object) 1)
    (setf (display-list object) nil))
  (loop for (nil start length) in (extra-display-lists object)
        do (opengl:gl-delete-lists start length))
  (setf (extra-display-lists object) nil))


;;; ------------------------------------------------------------
;;; Class projection
;;; 
;;; A class which defines the fovy, aspect, near and far
;;; values for a call to glu-perspective to define the projection
;;; matrix.

(defparameter *fovy* 45.0d0)
(defparameter *aspect* 1.0d0)
(defparameter *near* 1.0d0)
(defparameter *far* 100.0d0)


(defclass projection (object)
   ((fovy :initform *fovy* :initarg :fovy :accessor fovy)
    (aspect :initform *aspect* :initarg :aspect :accessor aspect)
    (near :initform *near* :initarg :near :accessor near)
    (far :initform *far* :initarg :far :accessor far)))

(defmethod draw ((projection projection))
  (opengl:glu-perspective (fovy projection) (aspect projection) (near projection) (far projection)))

(defun make-projection (&key fovy aspect near far)
  (make-instance 'projection
                 :fovy (or fovy *fovy*)
                 :aspect (or aspect *aspect*)
                 :near (or near *near*)
                 :far (or far *far*)))
                                       

;;; ------------------------------------------------------------
;;; Class camera
;;; 
;;; Defines an eye point, a center point and an up vector.
;;; The draw method calls GLU-LOOK-AT to install the camera values.

(defparameter *eye* (make-xyz :y 5.0d0))
(defparameter *center* (make-xyz))
(defparameter *up* (make-xyz :z  1.0d0))

(defclass camera (object)
  ((eye :initform (copy-structure *eye*)
        :initarg :eye
        :accessor eye
        :type xyz)
   (center :initform (copy-structure *center*)
           :initarg :center
           :accessor center
           :type xyz)
   (up :initform (copy-structure *up*)
       :initarg :up
       :accessor up
       :type xyz)
   (projection :initform (make-projection)
               :initarg :projection
               :accessor projection)))


(defmethod draw ((camera camera))
  (let ((eye (eye camera))
        (center (center camera))
        (up (up camera))
        (projection (projection camera)))
    (declare (type xyz up eye center))
    (opengl:gl-matrix-mode opengl:*gl-projection*)
    (opengl:gl-load-identity)
    (draw projection)
    (opengl:gl-matrix-mode opengl:*gl-modelview*)
    (opengl:gl-load-identity)
    (opengl:glu-look-at (xyz-x eye) (xyz-y eye) (xyz-z eye)
                        (xyz-x center) (xyz-y center) (xyz-z center)
                        (xyz-x up) (xyz-y up) (xyz-z up))
    (opengl:gl-enable opengl:*gl-lighting*)
    (opengl:gl-clear-color 0.0 0.0 0.3  1.0)
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)
    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
    (opengl:gl-depth-func opengl:*gl-less*)
    (opengl:gl-enable opengl:*gl-depth-test*)))

(defun make-camera (&key eye center up projection)
  (make-instance 'camera
                 :eye (copy-structure (or eye *eye*))
                 :center (copy-structure (or center *center*))
                 :up (copy-structure (or up *up*))
                 :projection (or projection (make-projection))))

