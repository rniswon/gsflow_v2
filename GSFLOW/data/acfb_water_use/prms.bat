@ECHO OFF
..\..\bin\gsflow .\control\control > .\output\screen.log
copy water_use.out .\output
del water_use.out
ECHO.
ECHO Run complete.
