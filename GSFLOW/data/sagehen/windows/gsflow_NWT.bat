@ECHO OFF

..\..\..\bin\gsflow_2.2 .\gsflow.control -set modflow_name ..\input\modflow\sagehen_NWT.nam -set subbasin_flag 0 -set print_debug 1 -set gsflow_output_file ..\output\gsflow_nwt.out
ECHO.
ECHO Run complete. Please press enter when you want to continue.
PAUSE>NUL