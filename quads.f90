      program fortran_stats

      implicit none
!Should be available in GitHub repository...
!User-entered variables
      integer            :: nsfcvars   =   0
      integer, parameter :: nvars      =   4
      integer, parameter :: xgridpts   = 540
      integer, parameter :: ygridpts   = 540
      integer, parameter :: zslices    =  86
      integer, parameter :: zgridpts   =  10
      integer, parameter :: ctrbuff    =  25       !Size of box
      integer, parameter :: xminbnd    = 235       !Westernmost dahndraft
      integer, parameter :: xmaxbnd    = 270
      integer, parameter :: yminbnd    = 240       !Southernmost dahndraft
      integer, parameter :: ymaxbnd    = 302       !Northernmost dahndraft
      integer, parameter :: nmin       =   1       !Start time
      integer, parameter :: nmax       =  61       !End time
      integer, parameter :: ffrlen     =  13
      integer, parameter :: offrlen    =  17
      integer, parameter :: pathlen    =  33
      integer, parameter :: outpathlen =  25
      integer, parameter :: statpathlen=  44
      integer, parameter :: threshold  =  12       !Min Surge Intensity
      integer, parameter :: region     =   1       !Storm Quadrant

      integer :: record(nvars) = (/20,21,22,25/) !Index 14=uinterp, 15=vinterp, 16=winterp

      real, parameter :: u0    = -12.00
      real, parameter :: v0    =  -3.00
      real, parameter :: umove =  -4.00
      real, parameter :: vmove =   1.25
    
      character(len=pathlen), parameter :: path   = &
!        '/lustre/work/mmahalik/delcity/'
         '/lustre/work/mmahalik/downdrafts/'            !When changing, make corresponding changes to pathlen param

      character(len=outpathlen), parameter :: outpath   = &
        '/lustre/scratch/mmahalik/'

!Other variables
      integer :: i, j, k, ii, irec, gridpts, surgecount, centerloc(2), &
        xc, yc, zc, n, boxwidth, xmin, xmax, ymin, ymax

      integer, allocatable :: surgearray(:)
      integer, allocatable :: centery(:)
      real, allocatable :: vortmax(:)
      real, allocatable :: srgmax(:)
      real, allocatable :: minw(:)
      real, allocatable :: maxw(:)

      integer :: &
        srgflag (xgridpts,ygridpts,zgridpts)

       real :: &
        u       (xgridpts,ygridpts,zgridpts), &
        v       (xgridpts,ygridpts,zgridpts), &
        w       (xgridpts,ygridpts,zgridpts), &
        zeta    (xgridpts,ygridpts,zgridpts), &
        uprime  (xgridpts,ygridpts,zgridpts), &
        vprime  (xgridpts,ygridpts,zgridpts), &
        surge   (xgridpts,ygridpts,zgridpts), &
        value   (xgridpts,ygridpts,zgridpts,nvars), &
        percent, ucorrect, vcorrect

      character(len=19) :: ffname
      character(len=23) :: offname
      character(len=64) :: fname, outputfile, filename, outfname, &
        outfilename, stats
      character(len=statpathlen) :: statspath

      statspath = '/home/mmahalik/thesis/TEXT_OUTPUT/stats0.txt'

      write(statspath(statpathlen-4:statpathlen-4),'(i1)')region
      stats = statspath

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
        outfilename = outfname//char(region)
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

!Top Left Box
      vortmax(n) = 100.0 * &
        maxval(zeta(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,2))

      surge = 0.0
      irec = 1
      surgecount = 0
      gridpts = boxwidth * boxwidth
      ucorrect = -1.0 * (u0 - umove)
      vcorrect = -1.0 * (v0 - vmove)
      if (region == 1) then
        ymin = yc
        ymax = yc+ctrbuff
        xmin = xc-ctrbuff
        xmax = xc
      elseif (region == 2) then
        ymin = yc
        ymax = yc+ctrbuff
        xmin = xc
        xmax = xc+ctrbuff
      elseif (region == 3) then
        ymin = yc-ctrbuff
        ymax = yc
        xmin = xc
        xmax = xc+ctrbuff
      elseif (region == 4) then
        ymin = yc-ctrbuff
        ymax = yc
        xmin = xc-ctrbuff
        xmax = xc
      endif
      do k = 1, zgridpts
        do j = ymin, ymax
          do i = xmin, xmax
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
          do i = 1, xmin-1
            srgflag(i,j,k) = 0
          enddo
          do i = xmax+1, xgridpts
            srgflag(i,j,k) = 0
          enddo       
        enddo
        do j = 1, ymin-1
          do i = 1, xgridpts 
            srgflag(i,j,k) = 0
          enddo
        enddo
        do j = ymax+1, ygridpts
          do i = 1, xgridpts 
            srgflag(i,j,k) = 0
          enddo
        enddo
      enddo

      k=1
      srgmax(n) = &
        maxval(surge(xmin:xmax,ymin:ymax,2))
      maxw(n) = maxval(w(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,10))
      minw(n) = minval(w(xc-ctrbuff:xc+ctrbuff,yc-ctrbuff:yc+ctrbuff,10))


      percent = (real(surgecount) / real(gridpts)) * 100.0

      print *, 'Total number of surge-flagged grid points: '
      print *, surgecount, '/', gridpts, ' = ', percent, '%'
      print *, ''
      print *, srgflag(248,280,1)

      surgearray(n) = surgecount
      centery(n) = yc

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
