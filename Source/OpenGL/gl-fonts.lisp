(defparameter *font-character-set-size* 96)
(defparameter *font-lists* nil)

(setf *fonts-lists* nil)

(defun create-font-lists ()
  nil)

(defun destroy-font-lists ()
  (dolist (list *font-lists*)
    (opengl:gl-delete-lists *font-lists*)))

(defun gl-print (text)
  (opengl:gl-push-attrib opengl:*GL-ALL-ATTRIB-BITS*)
  (opengl:gl-list-base )
  (opengl:gl-call-lists (length text) opengl:*GL-UNSIGNED-BYTE* text)
  (opengl:gl-pop-attrib))



