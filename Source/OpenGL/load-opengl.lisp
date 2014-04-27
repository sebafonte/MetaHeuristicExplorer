
;;; STEP 2 FROM DOC.TXT
(let ((hcl:*packages-for-warn-on-redefinition*
       (remove "CAPI" hcl:*packages-for-warn-on-redefinition* :test 'string=)))
  (load "OPENGL:compile"))

;;; LOAD BASIC OPENGL DEFINITIONS
;(load (current-pathname "examples/arrows"))
;(load (current-pathname "examples/texture"))


