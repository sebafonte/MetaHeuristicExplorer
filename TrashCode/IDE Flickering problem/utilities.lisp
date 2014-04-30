
(defmethod equals ((a t) (b t))
  "Answers whether <a> equals <b>."
  (equal a b))

(defun my-round-to-3 (number)
  (if number
      (coerce (/ (truncate (* 1000 (coerce number 'float))) 1000) 'float)))

(defun my-round-to-2 (number)
  (coerce (/ (truncate (* 100 (coerce number 'float))) 100) 'float))

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