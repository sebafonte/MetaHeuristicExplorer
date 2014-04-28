
(defun register-capi-button-icons ()
  (gp:register-image-translation
   'global-button-icons-images
   #.(gp:read-external-image (current-pathname "Resources\\global-button-icons.bmp")
                             :transparent-color-index 1))
  (gp:register-image-translation
   'global-button-icons-images-24
   #.(gp:read-external-image (current-pathname "Resources\\global-button-icons-24.bmp")
                             :transparent-color-index 1))
  (gp:register-image-translation
   'global-button-icons-images-32
   #.(gp:read-external-image (current-pathname "Resources\\global-button-icons-32.bmp")
                             :transparent-color-index 1)))  