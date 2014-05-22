(defvar *updater* nil)
(defvar *drawer* nil)
(defvar *updater-mbox* nil)
(defvar *drawing-lock* nil)

(defvar *time-variable* 0)


;; Drawing specific
(defun opengl-refresh-interface (interface)
  (redisplay-canvas (graphic-part interface)))

;; Initialization / destruction
(defun start-updater ()
  (setf *updater-mbox* (mp:make-mailbox)
        *drawing-lock* (mp:make-lock :name "Drawing lock")
        *drawer* (mp:process-run-function 
                  "Drawer"
                  (list :mailbox *updater-mbox* :priority *default-updater-process-priority*)
                  #'drawer-process)
        *updater* (mp:process-run-function
                   "Updater"
                   (list :mailbox *updater-mbox* :priority *default-updater-process-priority*)
                   #'updater-process)))

(defun kill-updater ()
  (mp:process-kill *updater*)
  (mp:process-kill *drawer*))

(defun initialize-graphics-updater ()
  (start-updater))

(defun refresh-interface (interface)
  (opengl-refresh-interface interface))


;; #WORKING
(defun updater-process ()
  (loop
   (mp:with-lock 
       (*drawing-lock*)
     (when (mp:mailbox-peek *updater-mbox*)
         (let ((interface (mp:mailbox-read *updater-mbox*)))
           (capi:execute-with-interface interface 'refresh-interface interface))))
   (sleep 0.01)))

(defun drawer-process ()
  (loop
   (let ((mb (mp:process-mailbox mp:*current-process*)))
     (incf *time-variable* 0.1)
     (mp:with-lock 
         (*drawing-lock*)
       (dolist (interface (append *interface-editors* *interface-graphic-editors*))
         (mp:mailbox-send *updater-mbox* interface)))
     (mp:process-wait "Wait draw" (lambda () (mp:mailbox-empty-p *updater-mbox*)))
     (sleep 0.01))))

