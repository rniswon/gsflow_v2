
                            GSFLOW Sample Problems
                                 March 2020
 
Ten sample problems with GSFLOW data sets are provided in this subdirectory
to verify that GSFLOW is correctly installed and running on the user's system.
The sample problems also may be looked at as examples of how to use various
simulation, input, and ouput options. The Data and Parameter Files can 
be found in 'input' subdirectories. Files ending in 'day' refer to 
pre-processed climate-data files used with the PRMS climate_by_hru module.
Results for simulations can be found in 'output' subdirectories. Results run from
the development computer for simulations can be found in 'output-test' 
subdirectories and are intended for comparison purposes of results in the 
'output' subdirectories.

Sample problems:

1. sagehen: This sample GSFLOW model is for the Sagehen Creek Watershed and 
   is described in the original GSFLOW documentation (Markstrom and others, 2008,
   USGS TM 6-D1).

2. sagehen_restart: This sample is also for the Sagehen Creek Watershed and 
   demonstrates use of the GSFLOW restart option. The problem is described in
   Regan and others (2015, USGS TM 6-D3).

3. tahoe_restart: This sample GSFLOW model is for the Lake Tahoe watershed
   and demonstrates use of the GSFLOW restart option. The problem is described
   in Regan and others (2015, USGS TM 6-D3).

4. Ag_EP1a: This sample GSFLOW model is for the Sagehen Creek watershed
   and demonstrates use of the MODFLOW AG Package. The problem is described
   in Niswonger (2020, AG_Package_EM&S.pdf) included in the "doc" directory.

5. Ag_EP1b: This sample GSFLOW model is for the Sagehen Creek watershed
   and demonstrates use of the MODFLOW AG Package. The problem is described
   in Niswonger (2020, AG_Package_EM&S.pdf) included in the "doc" directory.

6. Ag_EP2a: This sample GSFLOW model is for the Sagehen Creek watershed
   and demonstrates use of the MODFLOW AG Package. The problem is described
   in Niswonger (2020, AG_Package_EM&S.pdf) included in the "doc" directory.

7. Ag_EP2b: This sample GSFLOW model is for the Sagehen Creek watershed
   and demonstrates use of the MODFLOW AG Package. The problem is described
   in Niswonger (2020, AG_Package_EM&S.pdf) included in the "doc" directory.

8. acfb_dyn_params: This sample model is for the Apalachicola-Chattahoochee-Flint 
   River Basin the watershed above the Chattahoochee River near Norcross, 
   Georgia and is described in LaFontaine and others (2013). This sample 
   illustrates running PRMS with a time-series of dynamic parameters input 
   to the dynamic_param_read module that is described in Regan and LaFontaine (2017).
   
9. acfb_water_use: This sample model is for the Apalachicola-Chattahoochee-Flint 
   River Basin the watershed above the Chattahoochee River near Norcross, 
   Georgia and is described in LaFontaine and others (2013). This sample 
   illustrates running PRMS with a time-series of water-use input to the 
   water_use_read module that is described in Regan and LaFontaine (2017).

10. Tazlina: A model of the Tazlina Basin in Alaske is provided for illustrative
   and educational purposes only. The glacier dynamics simulation method  is
   described in Van Beusekom and Viger (2015).

References:

LaFontaine, J.H., Hay, L.E., Viger, R.J., Markstrom, S.L., Regan, R.S., 
Elliott, C.M., and Jones, J.W., 2013, Application of the Precipitation-
Runoff Modeling System (PRMS) in the Apalachicola–Chattahoochee–Flint 
River Basin in the southeastern United States: U.S. Geological Survey 
Scientific Investigations Report 2013–5162, 118 p., accessed October 13, 
2016, at https://pubs.usgs.gov/sir/2013/5162/.

Markstrom, S.L., Regan, R.S., Hay, L.E., Viger, R.J., Webb, R.M.T., 
Payn, R.A., and LaFontaine, J.H., 2015, PRMS-IV, the precipitation-
runoff modeling system, version 4: U.S. Geological Survey Techniques 
and Methods, book 6, chap. B7, 158 p., http://dx.doi.org/10.3133/tm6B7.

Markstrom, S.L., Niswonger, R.G., Regan, R.S., Prudic, D.E., and
Barlow, P.M., 2008, GSFLOW--Coupled Ground-water and Surface-water
FLOW model based on the integration of the Precipitation-Runoff
Modeling System (PRMS) and the Modular Ground-Water Flow Model
(MODFLOW-2005): U.S. Geological Survey Techniques and Methods
6-D1, 240 p.

Niswonger, R.G., 2020, An Agricultural Water Use Package for MODFLOW 
and GSFLOW: Environmental Modelling and Software 125 (2020) 104617, 16 p.

Regan, R.S., and LaFontaine, J.H., 2017, Documentation of the dynamic 
parameter, water-use, stream and lake flow routing, and two summary 
output modules and updates to surface-depression storage simulation and 
initial conditions specification options with the Precipitation-Runoff 
Modeling System (PRMS): U.S. Geological Survey Techniques and Methods, 
book 6, chap. B8, 60 p., https://doi.org/10.3133/tm6B8.

Regan, R.S., Niswonger, R.G., Markstrom, S.L., and Barlow, P.M., 2015, 
Documentation of a restart option for the U.S. Geological Survey coupled 
groundwater and surface-water flow (GSFLOW) model: U.S. Geological Survey 
Techniques and Methods, book 6, chap. D3, 19 p., http://dx.doi.org/10.3133/tm6D3.

Van Beusekom, A.E., and Viger, R.J., 2015, A glacier runoff extension to the
Precipitation Runoff Modeling System, Journal of Geophysical Research: Earth 
Science, 21 p., https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015JF003789. 