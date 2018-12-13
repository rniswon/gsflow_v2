
               Lake Tahoe Watershed Restart Sample Problem for GSFLOW
                                 October 2016
 
This sample is for the Lake Tahoe Watershed and demonstrates use of the 
GSFLOW restart option. The problem is described in Regan and others (2015, 
USGS TM 6-D3).

Control and batch files are provided in the 'windows' subdirectory to run the 
problem in GSFLOW mode.

The batch files for this series of restart simulations run GSFLOW a total of 
16 times. The control file for this series of simulations is 'Tahoe.control.' 
The first (or spin-up) simulation runs from October 1, 1980, through September 
1, 1983. The next 14 simulations run from September 2, 1983, through January 
30, 1984. A final simulation is then run to calculate the 'continuous 
simulation' described in Regan and others (2015) from October 1, 1980, through
January 30, 1984.

Output for the series of simulations is provided in the 'output-test' sub-
directory. Note that an executable program located in the 'bin' directory named 
'CSV_merge.exe' merges all of the csv output files for the restart 
simulations into a single output file using information from the 'names.dat' 
file provided in the 'windows' subdirectory. Also note that the 'gsflow.log' 
file provided in the directory is for the 'continuous' simulation.

In order to reduce the size of the GSFLOW distribution file, the binary restart
files that are created during the restart process have been deleted, and are
not included in the 'output-test' directory. These files will appear in the
'output\MODFLOW' subdirectory with names 'restartdata#.out,' in which # refers 
to the restart simulation number, and in the 'output\prms' subdirectory with
names 'prms_ic_#,' in which # refers to the restart simulation number.


Reference:

Regan, R.S., Niswonger, R.G., Markstrom, S.L., and Barlow, P.M., 2015, 
Documentation of a restart option for the U.S. Geological Survey coupled 
groundwater and surface-water flow (GSFLOW) model: U.S. Geological Survey 
Techniques and Methods, book 6, chap. D3, 19 p., 
http://dx.doi.org/10.3133/tm6D3/.

