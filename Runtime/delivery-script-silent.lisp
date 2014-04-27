;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load (merge-pathnames "..\\Source\\system-deliver-silent.lisp" (pathname-location (current-pathname))))

(deliver 'deliver-initialize-system (merge-pathnames "GE025-silent-release.exe" (pathname-location (current-pathname))) 0
         :keep-pretty-printer t
         :keep-fasl-dump nil
         :keep-package-manipulation nil
         :keep-top-level nil
         :format nil
         :gf-collapse-tty-output nil
         :icon-file (merge-pathnames "explorer.ico" (pathname-location (current-pathname)))
         :interface :capi
		 :keep-debug-mode nil
		 :product-name "Genetic explorer"
		 ;:versioninfo '(())
         )
