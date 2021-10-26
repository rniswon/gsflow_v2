@ECHO OFF
..\..\bin\gsflow .\yampa.control -print
java -cp ..\..\dist\oui4.jar oui.mms.gui.Mms .\yampa.control
ECHO.
ECHO Run complete. Please press enter to continue.
PAUSE>NUL
