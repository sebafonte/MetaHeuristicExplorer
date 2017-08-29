
;; Task containers
(defvar *search-tasks*)
(defvar *search-subtasks*)
(defvar *search-subtasks-remote*)

;; Global texture manager
(defparameter *texture-manager* nil)

;; Default mp:process priority values
(defparameter *default-search-task-process-priority* -50000)
(defparameter *default-search-task-min-process-priority* -50000)
(defparameter *default-search-task-max-process-priority* 1000000)
(defparameter *default-updater-process-priority* -500000)

;; Logger
(defvar *logger*)

;; TCP interfaces 
(defparameter *tcp-default-timeout* 1)
(defparameter *tcp-long-timeout* 2)

;; Other 
(defparameter *open-gui-on-startup* t)
(defvar *object-pool* (make-instance 'object-pool :max-count 100))
(defvar *interface-editors* nil)
(defvar *default-instance-search-task* nil)
(defvar *time-variable* 0)

;; Lock for fitness evaluations
(defparameter *auxiliar-lock* nil)
(defparameter *parsergen-lock* (mp:make-lock :name "parsergen-lock"))


(defun register-capi-button-icons ()
  (gp:register-image-translation
   'global-button-icons-images
   #.(gp:read-external-image (current-pathname "Interface/Resources/global-button-icons.bmp")
                             :transparent-color-index 1))
  (gp:register-image-translation
   'global-button-icons-images-24
   #.(gp:read-external-image (current-pathname "Interface/Resources/global-button-icons-24.bmp")
                             :transparent-color-index 1))
  (gp:register-image-translation
   'global-button-icons-images-32
   #.(gp:read-external-image (current-pathname "Interface/Resources/global-button-icons-32.bmp")
                             :transparent-color-index 1)))

