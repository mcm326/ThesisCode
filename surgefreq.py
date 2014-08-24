import numpy as np
import math
import os
import matplotlib.pyplot as plt

###User-selected variables. 
###note: Select region, nmax and nmin equal to those in fortran_stats.f90 code.

path       = '/home/mmahalik/thesis/TEXT_OUTPUT/'
statsfile  = 'stats.txt'

nmax =  61 #Downdraft sim
#nmax = 167 #Full model run
nmin =   1 #Downdraft sim
#nmin =  41 #Full model run

###Begin actual computation

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

spaces = np.arange(nmin-1,nmax)
time = spaces*30.0
length=len(time)-1
print length

fftx = np.fft.fft(points)
fft_shift = np.abs(np.fft.fftshift(fftx))

plt.subplot(231)
plt.bar(time, points, width=15)
plt.title('Number of Grid Points in a Surge ')
plt.xlabel('Time (s)')
plt.ylabel('Number of Grid Points')
plt.xlim([time[0],time[length]])
plt.grid()

plt.subplot(232)
plt.plot(time, ctrlocs)
plt.title('Location of Downdraft Center')
plt.xlabel('Time (s)')
plt.ylabel('Horizontal Grid Point: North --> ')
plt.xlim([time[0],time[length]])
plt.grid()

plt.subplot(233)
plt.plot(time, srgmax)
plt.title('Maximum Surge Intensity')
plt.xlabel('Time (s)')
plt.ylabel('Surge Index')
plt.xlim([time[0],time[length]])
plt.grid()

plt.subplot(234)
plt.plot(time, vortmax)
plt.title('Maximum Vertical Vorticity')
plt.xlabel('Time (s)')
plt.ylabel(r'$\zeta \times 10^2 (s^{-1})$')
plt.xlim([time[0],time[length]])
plt.grid()

plt.subplot(235)
plt.plot(time, updraft)
plt.title('Maximum 1km Updraft')
plt.xlabel('Time (s)')
plt.ylabel(r'w $(m {s^-1})$')
plt.xlim([time[0],time[length]])
plt.grid()

plt.subplot(236)
plt.plot(time, downdraft*-1.0)
plt.title('Maximum 1km Downdraft')
plt.xlabel('Time (s)')
plt.ylabel(r'w $(m {s^-1})$')
plt.xlim([time[0],time[length]])
plt.grid()
plt.show()

plt.plot(time, downdraft*-1.0)
plt.plot(time, updraft)
plt.title('Maximum 1km Vertical Motion')
plt.xlabel('Time (s)')
plt.ylabel(r'w $(m s{^-1})$')
plt.xlim([time[0],time[length]])
plt.legend(['Min w','Max w'])
plt.grid()
plt.show()

plt.plot(np.abs(fftx))
plt.show()

fig = plt.figure()
ax = fig.add_subplot(111)
lns1 = ax.fill_between(time, points/100.0, label='Grid Pts', facecolor='b')
lns2 = ax.plot(time, vortmax, label=r'$\zeta$', color='y', linewidth=3)
plt.title('Surge Progression, Vorticity, w')
ax2 = ax.twinx()
lns3 = ax2.plot(time, downdraft*-1.0, label='-w', color='r', linewidth=3)
lns = lns2+lns3
labs = [l.get_label() for l in lns]
ax.set_xlabel('Time (s)')
ax.set_ylabel(r'Maximum Vertical Vorticity $(\zeta)$ $(10^{-2} s^{-1})$')
ax2.set_ylabel(r'Downdraft Velocity $(m s^{-1})$')
ax.set_ylim([0,12])
plt.xlim([time[0],time[length]])
ax.legend(lns,labs)
ax.grid()
plt.show()

