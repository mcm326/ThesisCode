import numpy as np
import math
import os
import matplotlib.pyplot as plt


#-------------------------------------------------------------------------
### User-selected parameters
###note: Select region, nmax and nmin equal to those in fortran_stats.f90 code.

nregions =   4
nmax     = 167
nmin     =  40


#-------------------------------------------------------------------------
### Initiate each file to be read

regs = np.arange(1,nregions+1,1)

spaces = np.arange(nmin-1,nmax)
time = spaces*30.0
length=len(time)-1
print length

for region in regs:
    path       = '/home/mmahalik/thesis/TEXT_OUTPUT/'
    statsfile  = 'stats'+str(region)+'.txt'

    print 'Now computing statistics for Storm Region ', region


#-------------------------------------------------------------------------
### Begin reading statistics from stats txt file 

    statsloc  = path + statsfile
    stats     = np.genfromtxt(statsloc)

    points    = stats[0]
    ctrlocs   = stats[1]
    vortmax   = stats[2]
    srgmax    = stats[3]
    updraft   = stats[4]
    downdraft = stats[5]

    print len(points)
    print len(ctrlocs)
    print len(srgmax)
    print len(vortmax)
    print len(updraft)
    print len(downdraft)


#-------------------------------------------------------------------------
### Produce statistics plots for each region

    plt.subplot(2,2,1)
    plt.suptitle('Summary Surge Statistics in Storm Region '+str(region),fontsize=20)
    plt.bar(time, points, width=15)
    plt.title('Grid Points in a Surge')
    plt.xlabel('Time [s]')
    plt.ylabel('Number of Grid Points')
    plt.xlim([time[0],time[length]])
    plt.grid()

    plt.subplot(2,2,2)
    plt.plot(time, ctrlocs)
    plt.title('Location of Downdraft Center')
    plt.xlabel('Time [s]')
    plt.ylabel('Horizontal Grid Point: North --> ')
    plt.xlim([time[0],time[length]])
    plt.grid()

    plt.subplot(2,2,3)
    plt.plot(time, srgmax)
    plt.title('Maximum Surge Intensity')
    plt.xlabel('Time [s]')
    plt.ylabel('Surge Index')
    plt.xlim([time[0],time[length]])
    plt.grid()

    plt.subplot(2,2,4)
    plt.plot(time, vortmax)
    plt.title('Maximum Vertical Vorticity')
    plt.xlabel('Time [s]')
    plt.ylabel(r'$\zeta \times 10^-2 [s^-1]$')
    plt.xlim([time[0],time[length]])
    plt.grid()
    plt.show()


#-------------------------------------------------------------------------
### For reference, plot vertical velocity time series

plt.plot(time, updraft)
plt.plot(time, downdraft*-1.0)
plt.title('Maximum 1km Storm Updraft and Downdraft Velocity')
plt.xlabel('Time [s]')
plt.ylabel('w [m s^-1]')
plt.xlim([time[0],time[length]])
plt.legend(['Max w', 'Min w'])
plt.grid()
plt.show()

### End of program ###
