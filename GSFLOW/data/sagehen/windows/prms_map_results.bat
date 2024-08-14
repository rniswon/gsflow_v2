@ECHO OFF

..\..\..\bin\gsflow .\prms_map_results.control > ..\output\prms\screen_prmsonlyMAP.log
copy recharge.monthly ..\output\prms\
copy recharge.total ..\output\prms\
copy recharge.yearly ..\output\prms\
del recharge.*
ECHO.
ECHO Run complete. 
