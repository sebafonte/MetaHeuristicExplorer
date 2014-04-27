;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load (merge-pathnames "..\\Source\\system-deliver.lisp" (pathname-location (current-pathname))))

(deliver 'deliver-initialize-system (merge-pathnames "GE025-release.exe" (pathname-location (current-pathname))) 0
         :keep-pretty-printer t
         :keep-fasl-dump nil
         :keep-package-manipulation nil
         :keep-top-level nil
         :keep-debug-mode nil
         :format nil
         :gf-collapse-tty-output nil
         :icon-file (merge-pathnames "explorer.ico" (pathname-location (current-pathname)))
         :interface :capi
         :startup-bitmap-file (merge-pathnames "splash001.bmp" (pathname-location (current-pathname)))
         :product-name "Genetic explorer"
         ;:versioninfo '(())
         )
