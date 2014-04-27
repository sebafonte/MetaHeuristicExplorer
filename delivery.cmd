:: CONFIGURATION SECTION
SET RUNTIMEDIR=d:\explorer\Runtime
SET LISPWORKSDIR="c:\Program Files (x86)\LispWorks\lispworks-5-1-0-x86-win32.exe"

:: Delete old runtime files
DEL "Runtime\GE*.exe"

:: Generate new runtime files
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\delivery-script.lisp"
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\delivery-script-development.lisp"
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\delivery-script-silent.lisp"
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\delivery-script-silent-development.lisp"

