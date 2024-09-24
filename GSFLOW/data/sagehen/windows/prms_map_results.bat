@ECHO OFF

..\..\..\bin\gsflow .\prms_map_results.control > ..\output\prms\screen_prmsonlyMAP.log
move /y recharge.monthly ..\output\prms\
move /y recharge.total ..\output\prms\
move /y recharge.yearly ..\output\prms\
ECHO.
ECHO Run complete. 
