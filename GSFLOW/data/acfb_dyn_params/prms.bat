@ECHO OFF
..\..\bin\gsflow .\control\control > .\output\screen.log
copy dynamic_soil_parameter.out .\output
del dynamic_soil_parameter.out
ECHO.
ECHO Run complete.
