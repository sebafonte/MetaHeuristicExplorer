
;;; Invokes FUNCTION on ARGS in the process of INTERFACE like CAPI:EXECUTE-WITH-INTERFACE
;;; but always queues the execution request. Consequently, if this is called from code
;;; which is already running in INTERFACE's process, the execution of FUNCTION will be
;;; deferred until the interface is idle.
(defun execute-with-interface-when-idle (interface function &rest args)
  (let ((process (capi-internals:interface-process interface)))
    (if process
        (mp:process-send 
         process 
         (list function))
      (funcall function))))

;(defun execute-with-interface-when-idle (interface function &rest args)
;  (funcall function))

;; when idle
(defun initialize-opengl-refresh ()
  (dolist (i *interface-editors*)
    (opengl-refresh-interface i)))

(defun opengl-refresh-interface (interface)
  (execute-with-interface-when-idle 
   interface
   (lambda () 
     (capi:execute-with-interface 
      (interface *main-pane*)
      (lambda (interface) (redisplay-canvas (graphic-part interface)))
      interface))))

;; ?
(defun initialize-opengl-refresh ()
  (dolist (i *interface-editors*)
    (opengl-refresh-interface i)))

(defun opengl-refresh-interface (interface)
  (capi:execute-with-interface 
      (interface *main-pane*)
      (lambda (interface) (redisplay-canvas (graphic-part interface)))
      interface))


