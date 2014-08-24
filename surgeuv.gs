'reinit'

rc = gsfallow("on")
eps = 0

* zeta > 0

*xl = 49.25 
*xr = 49.75
*yb = 81.0
*yt = 82.0

* zeta < 0

xl = 61.75
xr = 62.7
yb = 60.50
yt = 61.3


xwest= 40 
xeast= 60 
ysouth= 60 
ynorth= 80

* lateral display boundaries

xwest= xwest * 4
xeast= xeast * 4
ysouth= ysouth * 4
ynorth= ynorth * 4

z = 1
z1 = 0.05

uskip=1
scalevect=25

settime =1 

'open  /lustre/scratch/mmahalik/surgestats_s.ctl'
tstart = 0.0
deltat = 30.0

say tstart
say deltat
say settime
say

mintim = tstart + deltat * (settime - 1)

'set lev 'z1

'q dims'
zlin=sublin(result,4)
zz1=subwrd(zlin,9)

'set arrowhead 0.03'
nomouse='y'
stop=0

* ---------------------------------
* Starting loop 
* ---------------------------------

while(stop<1)

IF (eps = 1)
*  'enable print vert_vort_'mintim'.mf'
 'enable print test.mf'
ENDIF

'c'

say xwest' 'xeast

'set background 1'
'set t 'settime
'set y 'ysouth' 'ynorth
'set x 'xwest' 'xeast
'set mproj off'
'set grads off'
'set xlint 2'
'set ylint 2'
'set strsiz 0.2'

* -----------------------------------
* Upper Left
* -----------------------------------

'set xlopts 0'
'set ylopts 0'

'set z 1'

'set parea 1 10 1 7.5'

'zeta = ((cdiff(vinterp,x) - cdiff(uinterp,y)) / 500) * 100'
'pi = 3.1415926'
*'dbz = 10*(log(qr+qs+qg)+11)'

'uprime = abs(uinterp + 8.0)'
'vprime = abs(vinterp + 4.25)'

*IF (dbz > 10)
'magnitude = (sqrt((uprime*uprime) + (vprime*vprime))) -6.0'
*ENDIF
*IF (dbz <= 10)
*  'magnitude = 0'
*ENDIF

'color 4 17 0.4 -kind white->green->yellow->red'
'd magnitude'

'set z 'zz1

'set gxout contour'
'set cstyle 1'
'set ccolor 2'
'set clevs 0.2 0.6 1.0'
'set clab on'
'set cthick 7'
'd zeta'

'set z 'zz1
'set gxout contour'
'set clevs -10 -8 -6 -4 -2'
'set cthick 7'
'set cstyle 3'
'set ccolor 0'
'd winterp'
'set clab on'

'set gxout contour'
'set clevs 10'
'set cthick 6'
'set cstyle 1'
'set ccolor 0'
'set clab on'
*'d dbz'

'set gxout contour'
'set clevs 5 10 20 30 40'
'set cthick 7'
'set cstyle 1'
'set ccolor 0'
'set clab on'
'd winterp'

IF (1 = 1)
  'set z 1' 
  'set gxout contour'
  'set clevs -1'
  'set cthick 8'
  'set cstyle 1'
  'set ccolor 4'
  'set clab on'
 'd thpert'
ENDIF

'set z 'zz1

'set ccolor 0'
'set cthick 3'
'set arrscl 0.25 'scalevect
'd skip(uinterp,'uskip','uskip');vinterp'

'cbar_joda1 1 1 0.2 7.75'
'set strsiz 0.2'
'set string 0 bl 5'

'set z 'zz1
'q dims'
levlin=sublin(result,4)
levstr=subwrd(levlin,6)
'draw string 1.0 7.7 z='levstr*1000' m'

'draw string 8.5 7.7 t='mintim' s'

'set strsiz 0.085'
'draw string 0.05 2.4 m s-1'

'set string 0 tr 5'
'set strsiz 0.17'
'draw string 8.9 8.2 Internal Surge Index (shaded), SFC velocity, Zeta'

'set strsiz 0.1'
'draw string 6.0 0.5 x (km)'
'draw string 0.65 7.3 y (km)'

*'cbar.inverted 0.8 1'
*'set strsiz 0.15'
*'draw string 10.3 1.3 1E-2 s^-1'

if (nomouse = 'y')

  say '   '
  say '   '
  say 'What next?'
  say 'f   go forward one output time'
  say 'b   go backward one output time'
  say 'n   replot shifting 2 km northward'
  say 's   replot shifting 2 km southward'
  say 'e   replot shifting 2 km eastward'
  say 'w   replot shifting 2 km westward'
  say 'zu   replot shifting each panel up by one model level'
  say 'zd   replot shifting each panel down by one model level'
say 'i   replot zooming in'
say 'o   replot zooming out'
say 'm   replot with more dense vectors'
say 'l   replot with less dense vectors'
say 'g   replot with longer scale vector'
say 'h   replot with shorter scale vector'
say 'u   replot moving only the upper panel up by a model level'
say 'd  replot moving only the upper panel down by a model level'
say 'y   replot moving only the lower panel up by a model level'
say 'yy  replot moving only the lower panel down by a model level'
say 'r  save image'
say 'q   Continue with mouse'
say 'qq  quit (leave this routine, GrADS stays active)'

pull command

if (command='r')
  IF (mintim < 10)
    say 'Saving PNG graphic in current directory'
    'printim  UVsurgeindex_00000'mintim'.png x1400 y1000'
  endif
  if (mintim <100 & mintim >= 10)
    'printime UVsurgeindex_0000'mintim'.png x1400 y1000'
  endif
  if (mintim < 1000 & mintim >=100)
    'printim UVsurgeindex_000'mintim'.png x1400 y1000'
  endif
  if (mintim < 1000 & mintim >=100)
    'printim UVsurgeindex_00'mintim'.png x1400 y1000'
  endif
  if (mintim < 10000 & mintim >= 1000)
    'printim UVsurgeindex_0'mintim'.png x1400 y1000'
  endif
  if (mintim < 100000 & mintim >= 10000)
    'printim UVsurgeindex_'mintim'.png x1400 y1000'
  endif
endif

*    IF (zz1 < 10)
*       'printim vert_vort_00'zz1'.png x1400 y1000'
*    ELSE
*       'printim vert_vort_0'zz1'.png x1400 y1000'
*    ENDIF
*ENDIF

*endif

if(command='f')
  settime=settime+1
  mintim = mintim + deltat
endif
if(command='b')
  settime=settime-1
  mintim = mintim - deltat
endif

if(command='n')
  ysouth=ysouth+10
  ynorth=ynorth+10
endif
if(command='s')
  ysouth=ysouth-10
  ynorth=ynorth-10
endif
if(command='e')
  xwest=xwest+10
  xeast=xeast+10
endif
if(command='w')
  xwest=xwest-10
  xeast=xeast-10
endif
if(command='zu')
  zz1=zz1+1
  zz2=zz2+1
endif
if(command='zd')
  zz1=zz1-1
  zz2=zz2-1
endif
if(command='i')
  ysouth=ysouth+3
  ynorth=ynorth-3
  xwest=xwest+3
  xeast=xeast-3
endif
if(command='o')
  ysouth=ysouth-3
  ynorth=ynorth+3
  xwest=xwest-3
  xeast=xeast+3
endif
if(command='m')
  uskip=uskip-1
endif
if(command='l')
  uskip=uskip+1
endif
if(command='g')
  scalevect=scalevect-5
endif
if(command='h')
  scalevect=scalevect+5
endif
if(command='u')
  zz1=zz1+1
endif
if(command='y')
  zz2=zz2+1
endif
if(command='d')
  zz1=zz1-1
endif
if(command='yy')
  zz2=zz2-1
endif
if(command='qq')
  stop=1
endif

if(command='q')
*  say ' '
*  say 'Continue time scanning with mouse?'
*  pull command
*  if (command = n); stop=1; endif
*  if (command != n); nomouse='n'; endif
  nomouse = 'n'
endif

endif  * nomouse option

if (nomouse = 'n')

say  
say 'right click: forward; left click: backward' 
say 'click mouse wheel to return.'

*  [left button = 1, middle=2, right=3]
  'q pos'
  mousekey=subwrd(result,5)
  if (mousekey = 1) 
     settime=settime - 1 
     mintim = mintim - deltat
   endif
   if (mousekey = 2) 
     nomouse='y' 
  endif
  if (mousekey = 3)
    settime=settime + 1 
    mintim = mintim + deltat 
  endif

endif

endwhile

