[![GSFLOW CI](https://github.com/rniswon/gsflow_v2/actions/workflows/ci.yml/badge.svg)](https://github.com/rniswon/gsflow_v2/actions/workflows/ci.yml)

# GSFLOW: Coupled Groundwater and Surface-Water Flow Model

GSFLOW is a coupled Groundwater and Surface-water FLOW model based on the 
integration of the USGS Precipitation-Runoff Modeling System (PRMS-V) and the 
USGS Modular Groundwater Flow Model (MODFLOW-2005 and MODFLOW-NWT). GSFLOW was 
developed to simulate coupled groundwater/surface-water flow in one or more 
watersheds by simultaneously simulating flow across the land surface, within 
subsurface saturated and unsaturated materials, and within streams and lakes. 
Climate data consisting of measured or estimated precipitation, air 
temperature, and solar radiation, as well as groundwater stresses 
(such as withdrawals) and boundary conditions are the driving factors for a 
GSFLOW simulation.

GSFLOW operates on a daily time step. In addition to the MODFLOW 
variable-length stress period used to specify changes in stress or boundary 
conditions, GSFLOW uses internal daily stress periods for adding recharge to 
the water table and calculating flows to streams and lakes. Specified stream 
inflow over boundaries, internal stream-diversion flow rates, and 
groundwater-pumping flow rates can be specified using time-series input files 
that allow these stresses to vary during each time step. 

GSFLOW can be used to evaluate the effects of such factors as land-use change, 
climate variability, and groundwater withdrawals on surface and subsurface flow 
for watersheds that range from a few square kilometers to several thousand 
square kilometers, and for time periods that range from months to several 
decades.


![image](https://user-images.githubusercontent.com/3373895/179845243-1e4c7f58-ef30-4081-aeb7-f332120d5f50.png)
