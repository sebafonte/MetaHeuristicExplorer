
(defun initialize-system ()
  "Execute all necessary functions to initialize system."
  (register-capi-button-icons)
  (initialize-default-logger)
  (initialize-locks)
  (initialize-event-handlers)
  (initialize-default-pane-positioners)
  (initialize-opencl)
  (initialize-system-configuration)
  (initialize-config-directories)
  (initialize-noise)
  (initialize-elite-managers)
  (initialize-selection-methods)
  (initialize-default-constant-factory)
  (initialize-genetic-operators)
  (initialize-default-grammars)
  (initialize-subroutine-compression-objects)
  (initialize-default-languages)
  (initialize-default-fitness-evaluators)
  (initialize-moea-objectives)
  (initialize-default-optimization-methods)
  (initialize-default-optimization-targets)
  (initialize-default-optimization-strategies)
  (initialize-default-population-generators)
  (initialize-default-breeders)
  (initialize-default-algorithms)
  (initialize-default-tcp-messages)
  (initialize-network-connections)
  (initialize-distributed-environment)
  (initialize-task-planifier)
  (initialize-main-server-informer-timer)
  (initialize-test-suites)
  (initialize-default-sample-objects)
  (initialize-default-applications)
  (initialize-auxiliar-objects)
  (initialize-texture-manager)
  (initialize-graphics-updater)
  (initialize-stack)
  (initialize-image-vector-functions)
  (initialize-default-search-task-object-templates))

(defun application-relative-pathname (file)
  (let* ((executable-name (car sys:*line-arguments-list*))
         (directory (pathname-location executable-name)))
    (merge-pathnames file directory)))

(defun initialize-config-directories ()
  (setf *default-configuration-path-benchmark-transfer-file*
        (application-relative-pathname "Configurations\\default-benchmark-transfer-file.explorer")
        *default-configuration-path-pane-explorer*
        (application-relative-pathname "Configurations\\default-pane-explorer.crossover-pane")
        *default-configuration-path-possible-remote-hosts*
        (application-relative-pathname (environment-file))
        *default-pane-subtasks-default-model*
        (application-relative-pathname "Configurations\\default-search-subtask.subtask") 
        *default-pane-tasks-default-model*
        (application-relative-pathname "Configurations\\default-search-task.task")))

(defun environment-file ()
  (format nil "Configurations\\~a" (or (network-environment-from-command-line) "possible-remote-hosts.hosts")))

(defun initialize-network-connections () 
  "Initialize network related objects."
  (system-add (default-local-connection-administrator)))

(defun initialize-task-planifier ()
  "Initialize the task planifier of the system.
   #Note: The default connection administrator needs to be initialized at this point."
  ;; Add default task planifiers
  (system-add 
   (default-local-only-task-planifier)
   (default-remote-only-task-planifier)
   (default-random-image-task-planifier))
  ;; Initialize variable that handles active processes count
  (setf *task-planifier-lock* (mp:make-lock :name "task-planifier-lock")))

(defun default-local-connection-administrator ()
  "Answer the system default connection administrator."
  (make-instance 'connection-administrator :name 'main-connection-administrator))

(defun default-local-only-task-planifier ()
  "Answer the default local only task planifier."
  (system-get 'global-running-image-planifier))

(defun default-remote-only-task-planifier ()
  "Answer the default remote only task planifier."
  (system-get 'global-remote-planifier))

(defun default-random-image-task-planifier ()
  (system-get 'global-random-task-planifier))

(defun load-default-panes () 
  "Load default pane configuration on main interface."
  nil)

(defun initialize-locks ()
  (setf *auxiliar-lock* (mp:make-lock :name "auxiliar-lock")))

(defun initialize-auxiliar-objects ()
  (setf *default-instance-search-task* (make-instance 'search-task))
  (system-add (make-instance 'command-line-interpreter :name 'main-command-line-interpreter)))

(defun initialize-command-line-settings ()
  (apply-command-line-parameters (system-get 'main-command-line-interpreter)))

(defun initialize-subroutine-compression-objects ()
  ;; Add default scoring strategies
  (system-add-with-name 
   (make-instance 'subroutine-equal-scoring-strategy)
   'default-subroutine-equal-scoring-strategy)
  (system-add-with-name 
   (make-instance 'subroutine-fitness-proportional-scoring-strategy)
   'default-subroutine-fitness-proportional-scoring-strategy)
  (system-add-with-name 
   (make-instance 'subroutine-frequency-proportional-scoring-strategy)
   'default-subroutine-frequency-proportional-scoring-strategy)
  ;; Add default subroutine replacement strategies
  (system-add-with-name 
   (make-instance 'no-limit-subroutine-replacement-strategy)
   'default-no-limit-subroutine-replacement-strategy)
  (system-add-with-name 
   (make-instance 'biased-call-subroutine-replacement-strategy)
   'default-biased-call-subroutine-replacement-strategy)
  (system-add-with-name 
   (make-instance 'subtree-subroutine-replacement-strategy)
   'default-subtree-subroutine-replacement-strategy)
  ;; Transfer from file
  (system-add-with-name 
   (make-instance 'file-subroutine-transfer-strategy :percentaje 100 :source "D:\\example.rtls")
   'default-file-subroutine-transfer-strategy)
  ;; Transfer from original task
  (system-add-with-name 
   (make-instance 'task-subroutine-transfer-strategy :percentaje 100)
   'default-task-subroutine-transfer-strategy)
  ;; Add default compression manager
  (system-add-with-name 
   (make-instance 'compression-subroutine-manager)
   'default-compression-subroutine-manager))

(defun initialize-texture-manager ()
  (setf *texture-manager* (make-instance 'gl-texture-manager))
  (register-texture-from-data *texture-manager* 'sample (temp-icosahedron-texture-data) 64 64 opengl:*gl-rgba*)
  (register-texture-from-data *texture-manager* 'sample7 (temp-sample-7-texture-data) 256 256 opengl:*gl-rgb*))

(defun initialize-stack ()
  ;; #DEBUG:
  (let ((length (hcl:extend-current-stack 0)))
    (hcl:extend-current-stack 1000)))