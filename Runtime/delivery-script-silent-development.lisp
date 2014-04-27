;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "d:/explorer/Source/system-deliver-silent.lisp")

(deliver 'deliver-initialize-system "d:/explorer/Runtime/GE025-silent-development.exe" 0
         :keep-pretty-printer t
         :keep-fasl-dump nil
         :keep-package-manipulation nil
         :keep-top-level nil
         :format nil
         :gf-collapse-tty-output nil
         :icon-file "d:/explorer/Runtime/explorer.ico"
         :interface :capi
		 :product-name "Genetic explorer"
		 ;:versioninfo '(())
         )
