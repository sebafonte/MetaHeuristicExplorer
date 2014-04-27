

(defmethod refresh-editors ((o opengl-with-capi))
  (purgue-invisible-editors)
  (dolist (editor *interface-editors*)
    (when (graphic-part editor)  
      (redisplay-canvas (graphic-part editor))))
  (dolist (editor *interface-graphic-editors*)
    (when (graphic-part editor)
      (redisplay-canvas (graphic-part editor)))))

(defun purgue-invisible-editors ()
  (setf *interface-editors* 
        (select 
         *interface-editors*
         (lambda (editor) (capi-internals:representation editor)))))

#|
(defmacro make-entity-explorer-pane-position (x y &rest args)
  "Default mutation-pane constructor macro with <args>."
  `(make-instance 
    'pane-editor-entity-explorer
    :interface-mode (mutation-editor-pane-interface-image-class *visualization-mode*)
    ,@args))

(capi:display
 (capi:make-container 
  (make-instance 'capi:editor-pane)
  :top-level-hook
  #'(lambda (func interface)
      (restart-case (funcall func)
        (nil ()
          :report 
          (list "Destroy Interface ~a" interface)
          (capi:destroy interface))))))
|#

(defmacro protect-interface-creation (args)
  `(restart-case
       (handler-bind ((error #'(lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'my-restart nil))))
         (progn ,@args))
     (my-restart (&optional v) v)))

#|
 (defun trap-error-handler (condition)
   (format *error-output* "~&~A~&" condition)
   (throw 'trap-errors nil))

 (defmacro trap-errors (&rest forms)
   `(catch 'trap-errors
      (handler-bind ((error #'trap-error-handler))
        ,@forms)))
 
 (list (trap-errors (signal "Foo.") 1)
       (trap-errors (error  "Bar.") 2)
       (+ 1 2))

(list (trap-errors (signal "Foo.") 1)
      (trap-errors (error  "Bar.") 2)
      (+ 1 2))

(sys:object-address x)

|#