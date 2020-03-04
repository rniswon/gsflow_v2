import sys, os
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import numpy as np
import pandas as pd
import datetime
from Figures import ReportFigures
from matplotlib import cm
from datetime import date
from datetime import timedelta
from dateutil.parser import parse
import matplotlib.dates as mdates
from dateutil.relativedelta import relativedelta

# Set for USGS report styes
rf = ReportFigures()
rf.set_style()
sys.path.append('..')
python_exe = sys.executable

# calculate number of lines in file can be any gage file
num_lines = 0
with open(r".\output\Agwater1GW_high.all", 'r') as f:
    for line in f:
        num_lines += 1

# set dates for daily values; this date is simulation starte date
dates = []
for i in range(num_lines-1): 
    dates.append(datetime.date(2014, 10, 1)+datetime.timedelta(days=i))
    
# set start and end dates for plot 
startdate, enddate, plotdates = [], [], []
startdate.append(datetime.date(2016, 10, 1))
enddate.append(datetime.date(2018, 9, 30))


#open irrigation well file again to read lines
fname = open(r".\output\Agwater1GW_high.all","r")
#set firstline for headers
firstline=fname.readline()
# store all data from file in lines
lines=fname.readlines()
x, y1_high, y2_high = [], [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lines:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        plotdates.append(dates[i])
        y1_high.append(line.split()[4])
        y2_high.append(line.split()[5])
# close irrigation well file
fname.close()

# Cumulate values and convert from cfs to acre-feet per acre
x=np.array(y1_high,dtype=float)
x=x*5.76e-4
y1_high=x
y1_high_cum=x.cumsum()
x=np.array(y2_high,dtype=float)
x=x*5.76e-4
y2_high=x
y2_high_cum=x.cumsum()

#print out average annual NIWR
print(y1_high_cum[len(y1_high_cum)-1]/2)
print(y2_high_cum[len(y2_high_cum)-1]/2)

# set headers for plot legend

header=[]
header.append(firstline.split()[0])
header.append(firstline.split()[1])
header.append(firstline.split()[2])
header.append(firstline.split()[3])
header.append(firstline.split()[4])
header.append(firstline.split()[5])
header.append(firstline.split()[6])

#open irrigation segment file again to read lines
fname = open(r".\output\Agwater1GW_low.all","r")
#set firstline for headers
firstline=fname.readline()
# store all data from file in lines
lines=fname.readlines()
y1_low, y2_low = [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lines:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
 #       plotdates.append(dates[i])
 #       x.append(line.split()[0])
        y1_low.append(line.split()[4])
        y2_low.append(line.split()[5])
# close irrigation well file
fname.close()

# Cumulate values and convert from cfs to acre-feet per acre
x1=np.array(y1_low,dtype=float)
x1=x1*5.76e-4
y1_low=x1
y1_low_cum=x1.cumsum()
x2=np.array(y2_low,dtype=float)
x2=x2*5.76e-4
y2_low=x2
y2_low_cum=x2.cumsum()

#print out average annual NIWR
print(y1_low_cum[len(y1_low_cum)-1]/2)
print(y2_low_cum[len(y2_low_cum)-1]/2)

#open SW ET file to read lines
fname = open(r".\output\Agwater1GW_ET_low.all","r")
#set firstline for headers
firstlineet=fname.readline()
# store all data from file in lines
lineset=fname.readlines()
y1et_low, y2et_low = [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lineset:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        #plotdates.append(dates[i])
#        xet.append(line.split()[0])
        y1et_low.append(line.split()[4])
        y2et_low.append(line.split()[5])

# Cumulate values
x1=np.array(y1et_low,dtype=float)
x1=x1*5.76e-4
y1et_low=x1
y1et_low_cum=x1.cumsum()
x2=np.array(y2et_low,dtype=float)
x2=x2*5.76e-4
y2et_low=x2
y2et_low_cum=x2.cumsum()

# close GW ET file
fname.close()

#print out average annual NIWR
print(y1et_low_cum[len(y1et_low_cum)-1]/2)
print(y2et_low_cum[len(y2et_low_cum)-1]/2)

#open SW ET file to read lines
fname = open(r".\output\Agwater1GW_high_ET.all","r")
#set firstline for headers
firstlineet=fname.readline()
# store all data from file in lines
lineset=fname.readlines()
y1et_high, y2et_high, y3et_high = [], [], []
# set variabes for plotting between startdate and enddate
i=-1
for line in lineset:
    i=i+1
    if dates[i]>enddate[0]:
        break
    if dates[i]>=startdate[0]:
        y1et_high.append(line.split()[4])
        y2et_high.append(line.split()[5])

# Cumulate values
x=np.array(y1et_high,dtype=float)
x=x*5.76e-4
y1et_high=x
y1et_high_cum=x.cumsum()
x=np.array(y2et_high,dtype=float)
x=x*5.76e-4
y2et_high=x
y2et_high_cum=x.cumsum()

# close GW ET file
fname.close()


#print out average annual NIWR
print(y1et_high_cum[len(y1et_high_cum)-1]/2)
print(y2et_high_cum[len(y2et_high_cum)-1]/2)

# blow up the fonts for this demo
plt.rcParams['xtick.labelsize'] = 8
plt.rcParams['ytick.labelsize'] = 8
plt.rcParams['axes.titlesize'] = 14

fig, axes = plt.subplots(1, 1, figsize=(10, 5))
#axes = axes.flat
lns1 = axes.plot(plotdates,y2_low,color='r', linewidth=2.5, label="Fine soil")
lns2 = axes.plot(plotdates,y2_high,'--', dashes=[10, 5, 10, 5], color='b', linewidth=2.5, label="Coarse soil")
ax2 = axes.twinx()
lns3 = ax2.plot(plotdates,y2_low_cum, color='purple', linewidth=2.5, label="Cumulative fine soil")
lns4 = ax2.plot(plotdates,y2_high_cum, color='orange', linewidth=2.5, label="Cumulative coarse soil")
lns5 = ax2.plot(plotdates,y2et_low_cum,'--', dashes=[10, 5, 10, 5],color='g', linewidth=2.5, label="Cumulative crop consumption")
axes.set_ylabel('Acre-feet per acre per day')
axes.set_xlabel(header[0])
ax2.set_ylabel('Acre-feet per acre', color='black')  # we already handled the x-label with ax1
# Set legend
lns = lns1+lns2+lns3+lns4+lns5
labs = [l.get_label() for l in lns]
axes.legend(lns, labs, loc=0, bbox_to_anchor=(.25, 0.95))

axes.set_xlabel(header[0])
plt.xlabel(header[0])

#rf.title(axes[0], 'Groundwater irrigation requirements for fine and coarse soil', subplot_prefix='A')
#rf.title(axes[1], 'Cumulative groundwater irrigation requirements for fine and coarse soil', subplot_prefix='B')

fmt = mdates.DateFormatter('%Y-%m-%d')

plt.tight_layout()
plt.savefig(r'.\output\Fig7.png')

