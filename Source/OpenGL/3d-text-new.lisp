(in-package "USER")


(defun ensure-set-up-gl-fonts (pane)
  (unless (font-lists (pane (capi:element-interface pane)))
    (set-up-gl-fonts pane)))

#+Win32
(defun set-up-gl-fonts (pane)
  (setf (font-lists (pane (capi:element-interface pane)))
        (list 
         :font (win32::wgl-use-font 
                pane 
                :start 0 
                :count 256 
                :outlinep t 
                :font (gp:find-best-font 
                       pane
                       (gp:make-font-description 
                        :family "Arial"
                        :size 250))) 
         256)))

#+Win32
(defmacro with-3d-text-state-saved (&body body)
  `(opengl:with-matrix-pushed
     (opengl:gl-push-attrib opengl:*gl-all-attrib-bits*)
     ,@body
     (opengl:gl-pop-attrib)))

(defmethod draw-opengl-on ((o graphic-function-r-r) canvas viewer)
  "Draws OpenGL scene <o> on <canvas>."
  (let* ((compiled-valuable (compiled-valuable o))
         (points (draw-data-gl o))
         (xmin (xmin o))
         (xmax (xmax o))
         (ymin (ymin o))
         (ymax (ymax o))
         (width (- xmax xmin))
         (height (- ymax ymin))
         (xmed (coerce (/ height 2) 'double-float))
         (x 0) 
         (y 0)
         (pane (pane (capi:element-interface canvas))))
    (declare (special x) (special y))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d xmin (- xmax xmin) (- ymax ymin) ymin)
      (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
      (opengl:gl-color3-f 1.0 1.0 1.0)
      (draw-lines-gl points ymax)
      (initialize-ortho-2d 0.0 (coerce width 'single-float) (coerce height 'single-float) 0.0)
      (ensure-set-up-gl-fonts canvas)
      (opengl:gl-color3-f 1.0 0.0 0.0)
      (draw-positioned-3d-text pane
                               (format nil "~A" xmin) 
                               0d0 xmed 0d0 0d0 180d0 0d0 1d0)
      (draw-positioned-3d-text pane
                               (format nil "~A" xmax)
                               (coerce (- width (* 4 (length (format nil "~A" xmax)))) 'double-float)
                               xmed 0d0 0d0 180d0 0d0 1d0)
      (draw-positioned-3d-text pane
                               (format nil "~A" ymin) 
                               0d0 
                               (coerce (- height 0.2) 'double-float) 
                               0d0 0d0 180d0 0d0 1d0)
      (draw-positioned-3d-text pane
                               (format nil "~A" ymax) 
                               0d0 
                               1d0
                               0d0 0d0 180d0 0d0 1d0)
      (opengl:swap-buffers canvas))))

#+Win32
(defun draw-positioned-3d-text (pane text x-pos y-pos z-pos x-rotation y-rotation z-rotation scale)
  (with-3d-text-state-saved
    (opengl:gl-translated x-pos y-pos z-pos)
    (opengl:gl-scaled (* 8 scale) scale scale)
    (opengl:gl-rotated x-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated y-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated z-rotation 0.0d0 0.0d0 1.0d0)
    (draw-3d-text pane text)))

#+Win32
(defun draw-3d-text (pane text)
  (let ((base (second (font-lists pane))))
    ;; Set up for a string-drawing display list call.
    (opengl:gl-list-base base)
    ;; Draw a string using font display lists.
    (fli:with-foreign-string (ptr elts bytes
                                  :external-format win32:*multibyte-code-page-ef*
                                  :null-terminated-p nil)
        text
      (declare (ignore bytes))
      (opengl:gl-call-lists elts opengl:*gl-unsigned-byte* ptr))))



#|
#+Win32
(defun draw-3d-text (pane text)
  (let ((base (second *font-lists*)))
    ;; Set up for a string-drawing display list call.
    (opengl:gl-list-base base)
    ;; Draw a string using font display lists.
    (fli:with-foreign-string (ptr elts bytes
                                  :external-format win32:*multibyte-code-page-ef*
                                  :null-terminated-p nil)
        text
      (declare (ignore bytes))
      (opengl:gl-call-lists elts opengl:*gl-unsigned-byte* ptr))))

#+Win32
(defun draw-positioned-3d-text (pane text x-pos y-pos z-pos x-rotation y-rotation z-rotation scale)
  (with-3d-text-state-saved
    (opengl:gl-translated x-pos y-pos z-pos)
    (opengl:gl-scaled scale scale scale)
    (opengl:gl-rotated x-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated y-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated z-rotation 0.0d0 0.0d0 1.0d0)
    (draw-3d-text pane text)))

(defmethod draw-opengl-on ((o graphic-function-r-r) canvas viewer)
  "Compute pixmap values into pixmap of <o>."
  (let* ((compiled-valuable (compiled-valuable o))
         (points (draw-data-gl o))
         (xmin (xmin o))
         (xmax (xmax o))
         (ymin (ymin o))
         (ymax (ymax o))
         (width (- xmax xmin))
         (height (- ymax ymin))
         (x 0) 
         (y 0))
    (declare (special x) (special y))
    (opengl:rendering-on (canvas)
      (initialize-ortho-2d xmin (- xmax xmin) (- ymax ymin) ymin)
      (opengl:gl-clear opengl:*GL-COLOR-BUFFER-BIT*)
      (opengl:gl-color3-f (coerce 1 'single-float) (coerce 1 'single-float) (coerce 1 'single-float))
      (draw-lines-gl points ymax)
      (ensure-set-up-gl-fonts canvas)
      (opengl:gl-color3-f (coerce 1 'single-float) (coerce 0 'single-float) (coerce 0 'single-float))
      (draw-positioned-3d-text pane (format nil "~A" xmin) 1d0 (coerce (/ height 2) 'double-float) 0d0 0d0 180d0 0d0 2d0)
      (draw-positioned-3d-text pane (format nil "~A" xmax)
                               (coerce (- width (1+ (length (format nil "~A" xmax)))) 'double-float)
                               (coerce (/ height 2) 'double-float) 0d0 0d0 180d0 0d0 2d0)
      (draw-positioned-3d-text pane (format nil "~A" ymin) 
                               1d0 
                               (coerce (- height 0.2) 'double-float) 
                               0d0 0d0 180d0 0d0 2d0)
      (draw-positioned-3d-text pane (format nil "~A" ymax) 
                               1d0 
                               1.5d0
                               0d0 0d0 180d0 0d0 2d0)
      (opengl:swap-buffers canvas))))
|#