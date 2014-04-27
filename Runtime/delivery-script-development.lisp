;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "d:/explorer/Source/system-deliver.lisp")

(deliver 'deliver-initialize-system "d:/explorer/Runtime/GE025-development.exe" 0
         :keep-pretty-printer t
         :icon-file "d:/explorer/Runtime/explorer.ico"
         :interface :capi
         :startup-bitmap-file "d:/explorer/Runtime/splash001.bmp"
		 :product-name "Genetic explorer"
		 ;:versioninfo '(())
         )
