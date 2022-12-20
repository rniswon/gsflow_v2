@ECHO OFF

..\..\..\bin\gsflow .\prms.control -set init_vars_from_file 0 -set save_vars_to_file 0 -set model_output_file ..\output\prms\prms_only.out > ..\output\prms\screen_prmsonly.log
ECHO.
ECHO Run complete.
