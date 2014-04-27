;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load (merge-pathnames "..\\Source\\system-deliver.lisp" (pathname-location (current-pathname))))

(deliver 'deliver-initialize-system (merge-pathnames "GE025-development.exe" (pathname-location (current-pathname))) 0
         :keep-pretty-printer t
         :icon-file "d:/explorer/Runtime/explorer.ico"
         :interface :capi
         :startup-bitmap-file "d:/explorer/Runtime/splash001.bmp"
		 :product-name "Genetic explorer"
		 ;:versioninfo '(())
         )
