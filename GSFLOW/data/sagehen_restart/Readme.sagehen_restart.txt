
                  Sagehen Restart Sample Problem for GSFLOW
                                 October 2016
 
This sample is for the Sagehen Creek Watershed and demonstrates use of the 
GSFLOW restart option. The problem is described in Regan and others (2015, 
USGS TM 6-D3).

Control and batch files are provided in the 'windows' subdirectory to run the 
problem in three model modes. 

1. GSFLOW mode: The batch file for this series of restart simulations 
('gsflow.bat') runs GSFLOW a total of 16 times, and its structure differs 
somewhat from the description of the restart process given in Regan and others 
(2015). The control file for this simulation is 'gsflow.control.' The 
first simulation runs the entire simulation period from October 1, 1980, 
through January 30, 1984, as described in the report. This simulation provides 
results for the entire simulation period (the 'continuous simulation' described
in Regan and others, 2015). GSFLOW is then run 15 additional times to
reproduce the restart simulations described in the report. The first of these
simulations is the 'hindcast' simulation, which runs from October 1, 1980,
through September 1, 1983. Note that for this run, the simulation 'end_time'
has been reset to '1983,9,1,0,0,0' in the batch file; several other input-
control parameters also are reset for this and each of the subsequent runs.

Output for the series of simulations is provided in the 'output-test' sub-
directory ('1_GSFLOW_mode'). Note that an executable program located in the
'bin' directory named 'CSV_merge.exe' merges all of the csv output
files for the restart simulations into a single output file using information 
from the 'names.dat' file provided in the 'windows' subdirectory. Also note that 
the 'gsflow.log' file provided in the directory is for the very last restart 
simulation that extends from December 2, 1983, through January 30, 1984. In
order to reduce the size of the GSFLOW distribution file, the binary restart
files that are created during the restart process have been deleted, and are
not included in the 'output-test' directory. These files will appear in the
'input\modflow' subdirectory with names 'restartdata#.out,' in which # refers 
to the restart simulation number, and in the 'output\prms' subdirectory with
names 'prms_ic_#,' in which # refers to the restart simulation number.

2. PRMS mode: The batch file for this simulation is 'gsflow_prms.bat' and the
control file is 'gsflow_prms.control.' Note that control parameter 'model_mode' is
reset from a value of GSFLOW5 to PRMS5 in the batch file. This simulation extends
from October 1, 1980, through January 30, 1984. Output for the simulation is 
provided in the '2_PRMS_model' subdirectory under the 'output-test' subdirectory.

3. MODFLOW mode: The batch file for this simulation is 'gsflow_modflow.bat' and 
the control file is 'gsflow.modflow.control.' Note that although the value of 
'start_time' is set to October 1, 1980, and the value of 'end_time' to January 
30, 1984, in the control file, the simulation actually extends through September 
30, 1996, because the number of time steps specified for the second stress period 
of this simulation in the discretization file is 5,844 days; this demonstrates 
that for a MODFLOW-only simulation, the discretization file time information 
takes precedence. Output for the simulation is provided in the '3_MODFLOW_model'
subdirectory under the 'output-test' subdirectory.


Reference:

Regan, R.S., Niswonger, R.G., Markstrom, S.L., and Barlow, P.M., 2015, 
Documentation of a restart option for the U.S. Geological Survey coupled 
groundwater and surface-water flow (GSFLOW) model: U.S. Geological Survey 
Techniques and Methods, book 6, chap. D3, 19 p., 
http://dx.doi.org/10.3133/tm6D3/.

