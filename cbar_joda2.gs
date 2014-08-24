*
*  Script to plot a colorbar
*
*  The script will assume a colorbar is wanted even if there is 
*  not room -- it will plot on the side or the bottom if there is
*  room in either place, otherwise it will plot along the bottom and
*  overlay labels there if any.  This can be dealt with via 
*  the 'set parea' command.  In version 2 the default parea will
*  be changed, but we want to guarantee upward compatibility in
*  sub-releases.
*
*
*	modifications by mike fiorino 940614
*
*	- the extreme colors are plotted as triangles
*	- the colors are boxed in white
*	- input arguments in during a run execution:
* 
*	run cbarn sf vert xmid ymid units
*
*	sf   - scale the whole bar 1.0 = original 0.5 half the size, etc.
*	vert - 0 FORCES a horizontal bar = 1 a vertical bar
*	xmid - the x position on the virtual page the center the bar
*	ymid - the x position on the virtual page the center the bar
**French units - 0=none;1=cape (j/kg);2=dbz;3=m/s;4=C;5=K;6m2s2)
*	if vert,xmid,ymid are not specified, they are selected
*	as in the original algorithm
*  

function cbar (args)

sf=subwrd(args,1)
vert=subwrd(args,2)
xmid=subwrd(args,3)
*xmid= 9.45
ymid=subwrd(args,4)
units=subwrd(args,5)

if(sf='');sf=1.0;endif

*
*  Check shading information
*
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None') 
    say 'Cannot plot color bar: No shading information'
    return
  endif

* 
*  Get plot size info
*
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo = subwrd(rec4,4)
  xhi = subwrd(rec3,6)
  xd = xsiz - xhi

  ylolim=0.6*sf
  xdlim1=1.0*sf
  xdlim2=1.5*sf  
  barsf=0.8*sf
  yoffset=0.2*sf
** dahl 0.02 (was: 0.1)
  stroff=0.02*sf
  strxsiz=0.12*sf
  strysiz=0.13*sf
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<ylolim & xd<xdlim1) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
*
*	logic for setting the bar orientation with user overides
*
  if (ylo<ylolim | xd>xdlim1)
    vchk = 1
    if(vert = 0) ; vchk = 0 ; endif
  else
    vchk = 0
    if(vert = 1) ; vchk = 1 ; endif
  endif
*
*	vertical bar
*

  if (vchk = 1 )

    if(xmid = '') ; xmid = xhi+xd/2 ; endif
** dahl 0.1
    xwid = 0.1*sf
    ywid = 0.8*sf
    
    xl = xmid-xwid/2
    xr = xl + xwid
    if (ywid*cnum > ysiz*barsf) 
** dahl 1.5
      ywid = 1.5*ysiz*barsf/cnum
    endif
    if(ymid = '') ; ymid = ysiz/2 ; endif
    yb = ymid - ywid*cnum/2
    'set string 1 l 5'
    vert = 1

  else

*
*	horizontal bar
*

    ywid = 0.4
    xwid = 0.8

    if(ymid = '') ; ymid = ylo/2-ywid/2 ; endif
    yt = ymid + yoffset
    yb = ymid
    if(xmid = '') ; xmid = xsiz/2 ; endif
    if (xwid*cnum > xsiz*barsf)
      xwid = xsiz*barsf/cnum
    endif
    xl = xmid - xwid*cnum/2
    'set string 1 tc 5'
    vert = 0
  endif


*
*  Plot colorbar
*


** dahl 1.2 --> 0.8
  'set strsiz '0.6*strxsiz' '0.6*strysiz
  num = 0
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col =  subwrd(rec,1)
    hi = subwrd(rec,3)
    if (vert) 
      yt = yb + ywid
    else 
      xr = xl + xwid
    endif

    if(num!=0 & num!= cnum-1)
    'set line 1 1 10'
    'draw rec 'xl' 'yb' 'xr' 'yt
    'set line 'col
    'draw recf 'xl' 'yb' 'xr' 'yt
    if (num<cnum-1)
      if (vert) 
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif
    endif

    if(num = 0 )

      if(vert = 1)

        xm=(xl+xr)*0.5
        'set line 1 1 10'
        'draw line 'xl' 'yt' 'xm' 'yb
        'draw line 'xm' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'yt
       'set line 'col
        'draw polyf 'xl' 'yt' 'xm' 'yb' 'xr' 'yt' 'xl' 'yt

      else

        ym=(yb+yt)*0.5
        'set line 1 1 10'
        'draw line 'xl' 'ym' 'xr' 'yb
        'draw line 'xr' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'ym

        'set line 'col
       'draw polyf 'xl' 'ym' 'xr' 'yb' 'xr' 'yt' 'xl' 'ym

      endif

    endif

    if (num=0)
      if (vert)
         xp=xr+stroff
      if(units>0)
        if(units=1)
        'draw string 'xp' 'yb' J kg`a-1`n' 
        endif
        if(units=2)
        'draw string 'xp' 'yb' dbz' 
        endif
        if(units=3)
        'draw string 'xp' 'yb' m s`a-1`n' 
        endif
        if(units=4)
        'draw string 'xp' 'yb' `ao`nC' 
        endif
        if(units=5)
        'draw string 'xp' 'yb' K' 
        endif
        if(units=5)
        'draw string 'xp' 'yb' m`a2`n s`a-2`n'  
        endif
       endif
      endif
     endif

    if (num<cnum-1)
      if (vert)
         xp=xr+stroff 
        'draw string 'xp' 'yt' 'hi
      else
         yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif

    if(num = cnum-1 )

      if( vert = 1)
        'set line 1 1 10'
        'draw line 'xl' 'yb' 'xm' 'yt
        'draw line 'xm' 'yt' 'xr' 'yb
        'draw line 'xr' 'yb' 'xl' 'yb
        'set line 'col
        'draw polyf 'xl' 'yb' 'xm' 'yt' 'xr' 'yb' 'xl' 'yb
      else

        'set line 1 1 10'
        'draw line 'xr' 'ym' 'xl' 'yb
        'draw line 'xl' 'yb' 'xl' 'yt
        'draw line 'xl' 'yt' 'xr' 'ym

        'set line 'col
        'draw polyf 'xr' 'ym' 'xl' 'yb' 'xl' 'yt' 'xr' 'ym
        

      endif

    endif

    if (num<cnum-1)
      if (vert) 
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
       'draw string 'xr' 'yp' 'hi
      endif
    endif

    if (num=cnum-1)
      if (vert) 
        xp=xr+stroff
      else
          yp=yb-stroff
      if(units>0)
        if(units=1)
        'draw string 'xr' 'yp' J kg`a-1`n' 
        endif
        if(units=2)
        'draw string 'xr' 'yp' dbz' 
        endif
        if(units=3)
        'draw string 'xr' 'yp' m s`a-1`n' 
        endif
        if(units=4)
        'draw string 'xr' 'yp' `ao`nC' 
        endif
        if(units=5)
        'draw string 'xr' 'yp' K' 
        endif
        if(units=6)
        'draw string 'xr' 'yp' m`a2`n s`a-2`n' 
        endif
       endif

      endif
    endif



    num = num + 3
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
return
