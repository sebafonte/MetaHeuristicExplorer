:: CONFIGURATION SECTION
SET LISPWORKSDIR="c:\Program Files (x86)\LispWorks\lispworks-5-1-0-x86-win32.exe"

:: Delete old runtime files
DEL "Runtime\GE*.exe"

:: Generate new runtime files
SET RUNTIMEDIR=%CD%
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\Runtime\delivery-script.lisp"
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\Runtime\delivery-script-development.lisp"
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\Runtime\delivery-script-silent.lisp"
CALL %LISPWORKSDIR% -init "%RUNTIMEDIR%\Runtime\delivery-script-silent-development.lisp"

