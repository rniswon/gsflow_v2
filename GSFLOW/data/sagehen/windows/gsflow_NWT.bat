@ECHO OFF

..\..\..\bin\gsflow .\gsflow.control -set modflow_name ..\input\modflow\sagehen_NWT.nam -set subbasin_flag 0 -set print_debug -2 -set gsflow_output_file ..\output\gsflow_nwt.out -set csv_output_file ..\output\gsflow_NWT.csv > ..\output\screenNWT.log
ECHO.
ECHO Run complete.