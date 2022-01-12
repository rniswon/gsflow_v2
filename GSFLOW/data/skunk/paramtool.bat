@ECHO OFF
..\..\bin\gsflow -C.\control\skunk.control -print
java -cp ..\..\dist\oui4.jar oui.paramtool.ParamTool .\input\skunk.params .\control\skunk.control.par_name
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
