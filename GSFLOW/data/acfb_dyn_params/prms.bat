@ECHO OFF
..\..\bin\gsflow .\control\control > .\output\screen.log
move /y dynamic_soil_parameter.out .\output
ECHO.
ECHO Run complete.
