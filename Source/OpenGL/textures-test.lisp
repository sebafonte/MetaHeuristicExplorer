

(defun load-texture (port image)
  (let ((image-access (gp:make-image-access port image)))
    (unwind-protect
	 (let* ((width (gp:image-access-width image-access))
		(height (gp:image-access-height image-access))
		(tw (* width 4)))
	   (gp:image-access-transfer-from-image image-access)
	   (opengl:with-gl-vectors ((data :type :single-float :length (* tw height)))
	     (declare #.*optimize*
		      (type fixnum tw width height)
		      (type gp:image image))
	     ;; Copy color channels and insert alpha
	     (loop for y fixnum from 0 below height
		for ty fixnum from 0 by tw do
		  (loop for x fixnum from 0 below width
		     for tx fixnum from ty by 4 
		     for tr fixnum = tx 
		     for tg fixnum = (1+ tr)
		     for tb fixnum = (1+ tg)
		     for ta fixnum = (1+ tb) do
		       (let* ((pixel (gp:image-access-pixel image-access x y))
			      (color (color:unconvert-color port pixel)))
			 (setf (opengl:gl-vector-aref data tr) 
			       (color:color-red color))
			 (setf (opengl:gl-vector-aref data tg) 
			       (color:color-green color))
			 (setf (opengl:gl-vector-aref data tb) 
			       (color:color-blue color))
			 (setf (opengl:gl-vector-aref data ta) 
			       1))))
	     ;; uild our texture mipmaps
	     (opengl:glu-build2-dmipmaps opengl:*gl-texture-2d* 
					 4 width height
					 opengl:*gl-rgba* 
					 opengl:*gl-float*
					 data)
                        ;; save texture name
	   (opengl:gl-enable opengl:*gl-texture-2d*)))
      (gp:free-image-access image-access))))
