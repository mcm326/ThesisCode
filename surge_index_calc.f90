      program fortran_stats

      implicit none

!User-entered variables
      integer            :: nsfcvars   =   0
      integer, parameter :: nvars      =   4
      integer, parameter :: xgridpts   = 540
      integer, parameter :: ygridpts   = 540
      integer, parameter :: zslices    =  86 
      integer, parameter :: zgridpts   =  10       !How many of 86 levels to read
      integer, parameter :: ctrbuff    =  70       !Size of box(ideal=25)
      integer, parameter :: xminbnd    =  71       !Westernmost downdraft 240/71
      integer, parameter :: xmaxbnd    = 469       !Easternmost downdraft 300/469
      integer, parameter :: yminbnd    =  71       !Southernmost downdraft 286/71
      integer, parameter :: ymaxbnd    = 469       !Northernmost downdraft 300/469
      integer, parameter :: nmin       =   1       !Start time -- Full:41 DDsim:1
      integer, parameter :: nmax       =  61       !End time -- Full:167 DDsim:61
      integer, parameter :: ffrlen     =  13
      integer, parameter :: offrlen    =  17
      integer, parameter :: pathlen    =  33       !Downdraft sim
!      integer, parameter :: pathlen    =  30       !Full model run
      integer, parameter :: outpathlen =  25
      integer, parameter :: threshold  =  12       !Min Surge Intensity

      integer :: record(nvars) = (/9,10,11,14/) !Index uinterp,vinterp,winterp,zvort from ctl file: DD
!      integer :: record(nvars) = (/20,21,22,25/) !Index uinterp,vinterp,winterp,zvort from ctl file: Full

      real, parameter :: u0    = -12.00
      real, parameter :: v0    =  -3.00
      real, parameter :: umove =  -4.00
      real, parameter :: vmove =   1.25

      character(len=pathlen), parameter :: path   = &
        '/lustre/work/mmahalik/downdrafts/'
!        '/lustre/work/mmahalik/delcity/'

      character(len=outpathlen), parameter :: outpath   = &
        '/lustre/scratch/mmahalik/'

      character(len=64), parameter :: stats = &
        '/home/mmahalik/thesis/TEXT_OUTPUT/stats.txt' 

!Other variables
      integer :: i, j, k, ii, irec, gridpts, surgecount, centerloc(2), &
        xc, yc, zc, n, boxwidth

      integer, allocatable :: surgearray(:)
      integer, allocatable :: centery(:)
      real, allocatable :: vortmax(:)
      real, allocatable :: srgmax(:)
      real, allocatable :: minw(:)
      real, allocatable :: maxw(:)

      integer :: &
        srgflag (xgridpts,ygridpts,zgridpts)

      real :: &
        uprime  (xgridpts,ygridpts,zgridpts), &
        vprime  (xgridpts,ygridpts,zgridpts), &
        surge   (xgridpts,ygridpts,zgridpts), &       
        u       (xgridpts,ygridpts,zgridpts), &
        v       (xgridpts,ygridpts,zgridpts), &
        w       (xgridpts,ygridpts,zgridpts), &
        zeta    (xgridpts,ygridpts,zgridpts), &
        value   (xgridpts,ygridpts,zgridpts,nvars), &
        percent, ucorrect, vcorrect

      character(len=19) :: ffname
      character(len=23) :: offname
      character(len=64) :: fname, outputfile, filename, outfname, &
        outfilename

      ffname = 'cm1out_000000_s.dat'
      offname = 'surgestats_000000_s.dat'

      outputfile = "/home/mmahalik/thesis/surgestats1.bin"

      boxwidth = 2 * ctrbuff + 1

!Dynamic file naming based on file timestamp
      fname  = path//ffname
      value = -99999.0

      allocate(surgearray(nmax))
      allocate(centery(nmax))
      allocate(vortmax(nmax))
      allocate(srgmax(nmax))
      allocate(minw(nmax))
      allocate(maxw(nmax))

      do n = nmin, nmax,1
        if (n < 10) then
          write(fname(pathlen+ffrlen:pathlen+ffrlen),'(i1)')n
        elseif (n < 100 .and. n >= 10) then
          write(fname(pathlen+ffrlen-1:pathlen+ffrlen),'(i2)')n
        elseif (n < 1000 .and. n >= 100) then
          write(fname(pathlen+ffrlen-2:pathlen+ffrlen),'(i3)')n
        elseif (n < 10000 .and. n >= 1000) then
          write(fname(pathlen+ffrlen-3:pathlen+ffrlen),'(i4)')n
        endif
        filename = fname
        print *, filename 

!choose file number that corresponds to number=time/30
       
        outfname = outpath//offname

        if (n < 10) then
          write(outfname(outpathlen+offrlen:outpathlen+offrlen),'(i1)')n
        elseif (n < 100 .and. n >= 10) then
          write(outfname(outpathlen+offrlen-1:outpathlen+offrlen), &
            '(i2)')n
        elseif (n < 1000 .and. n >= 100) then
          write(outfname(outpathlen+offrlen-2:outpathlen+offrlen), &
            '(i3)')n
        elseif (n < 10000 .and. n >= 1000) then
          write(outfname(outpathlen+offrlen-3:outpathlen+offrlen), &
            '(i4)')n
        endif
        outfilename = outfname
       print *, outfilename 


      open(20, file=filename, form="unformatted", access="direct", &
        recl=4*xgridpts*ygridpts)

      do ii = 1, nvars
        irec = nsfcvars * (1-zslices) + zslices*(record(ii)-1) + 1

        do k = 1, zgridpts
          read(20, rec=irec) ((value(i,j,k,ii), i=1, xgridpts), j=1, &
            ygridpts)
          irec = irec+1
        enddo

        if (ii==1) then
          u(:,:,:)    = value(:,:,:,ii)
        elseif (ii==2) then
          v(:,:,:)    = value(:,:,:,ii)
        elseif (ii==3) then
          w(:,:,:)    = value(:,:,:,ii)
        elseif (ii==4) then
          zeta(:,:,:) = value(:,:,:,ii)
        endif
      enddo

      close(20)

!Find location of main downdraft and center location there
      k = 10 

      centerloc = minloc(w(xminbnd:xmaxbnd,yminbnd:ymaxbnd,2))
      xc = xminbnd - 1 + centerloc(1)
      yc = yminbnd - 1 + centerloc(2)
      zc = k
      print *, 'Downdraft Center: ', xc,yc,zc

!Do statistical calculations here

      vortmax(n) = 100.0 * &
        maxval(zeta(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,2))

      surge = 0.0
      irec = 1
      surgecount = 0
      gridpts = boxwidth * boxwidth
      ucorrect = -1.0 * (u0 - umove)
      vcorrect = -1.0 * (v0 - vmove)
      do k = 1, zgridpts
        do j = yc-ctrbuff, (yc+ctrbuff)
          do i = xc-ctrbuff, (xc+ctrbuff)
            uprime(i,j,k) = u(i,j,k) + ucorrect
            vprime(i,j,k) = v(i,j,k) + vcorrect
            surge(i,j,k) = ((uprime(i,j,k)*uprime(i,j,k)) + &
              (vprime(i,j,k)*vprime(i,j,k)))**0.5
            if (surge(i,j,k) > threshold) then
              srgflag(i,j,k) = 1
                if (k==1) then
                  surgecount = surgecount + 1
                else
                  surgecount = surgecount
                endif
            else
              srgflag(i,j,k) = 0
            endif
          enddo
        enddo
        do j = 1, ygridpts
          do i = 1, xc-(ctrbuff+1)
            srgflag(i,j,k) = 0
          enddo
          do i = xc+ctrbuff, xgridpts
            srgflag(i,j,k) = 0
          enddo       
        enddo
        do j = 1, yc-(ctrbuff+1)
          do i = 1, xgridpts 
            srgflag(i,j,k) = 0
          enddo
        enddo
        do j = yc+(ctrbuff+1), ygridpts
          do i = 1, xgridpts 
            srgflag(i,j,k) = 0
          enddo
        enddo
      enddo
     
     print *, 'ALL VARS: '
     print *, u(250,250,10)
     print *, v(250,250,10)     
     print *, w(250,250,10)     
     print *, zeta(250,250,10)     

     print *, 'sample u,v,w,zvort: ', u(250,250,10), &
        v(250,250,10),w(250,250,10),zeta(250,250,10)
      k=1
      srgmax(n) = &
        maxval(surge(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,2))
      maxw(n) = maxval(w(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,10))
      minw(n) = minval(w(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,10))
      print *, 'sample srgmax,max/minw: ', srgmax(n), maxw(n), minw(n)

      percent = (real(surgecount) / real(gridpts)) * 100.0

      print *, 'Total number of surge-flagged grid points: '
      print *, surgecount, '/', gridpts, ' = ', percent, '%'
      print *, ''
      print *, srgflag(248,280,1)

      surgearray(n) = surgecount
      centery(n) = yc
      print *, surgearray(n)

!Write to output file here

      open(28, status='replace', file=outfilename, &
        form='unformatted', access='direct', recl=4*xgridpts*ygridpts)
  
      irec=1     
      do k=1, zgridpts
        write(28, rec=irec)((surge(i,j,k), i=1, xgridpts), j=1,ygridpts)
        irec=irec+1
      enddo
      do k=1, zgridpts
        write(28, rec=irec)((srgflag(i,j,k), i=1,xgridpts),j=1,ygridpts)
        irec=irec+1
      enddo      
      do k=1, zgridpts
        write(28, rec=irec)((u(i,j,k), i=1, xgridpts), j=1, ygridpts)
        irec=irec+1
      enddo      
      do k=1, zgridpts
        write(28, rec=irec)((v(i,j,k), i=1, xgridpts), j=1, ygridpts)
        irec=irec+1
      enddo
      do k=1, zgridpts
        write(28, rec=irec)((w(i,j,k), i=1, xgridpts), j=1, ygridpts)
        irec=irec+1
      enddo
      do k=1, zgridpts
        write(28, rec=irec)((zeta(i,j,k), i=1, xgridpts), j=1, ygridpts)
        irec=irec+1
      enddo
      close(28)
      enddo

      open(21, status='replace', file=stats)  
      write(21,*)(surgearray(n), n=nmin, nmax)
      write(21,*)(centery(n), n=nmin, nmax)
      write(21,*)(vortmax(n), n=nmin, nmax)
      write(21,*)(srgmax(n), n=nmin, nmax)
      write(21,*)(maxw(n), n=nmin, nmax)
      write(21,*)(minw(n), n=nmin, nmax)
      close(21)

   
      deallocate(surgearray)
      deallocate(centery)
      deallocate(vortmax)
      deallocate(srgmax)
      deallocate(maxw)
      deallocate(minw)

      print *, 'Done.'
      print *, ''
      print *, 'Calculated quantities have been printed into: ', &
        outfilename
      print *, ''
      print *, 'Matrix of satistics has been printed into: ', &
        stats
      print *, ''
      print *, 'Closing program. Hasta la vista.'

      end program
