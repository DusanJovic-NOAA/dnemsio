program dnemsio

  use nemsio_module

  implicit none

  integer :: iret

  ! meta1 (48 bytes)
  character(len=nemsio_charkind8)             :: gtype_1       ,gtype_2
  character(len=nemsio_charkind8)             :: gdatatype_1   ,gdatatype_2
  character(len=nemsio_charkind8)             :: modelname_1   ,modelname_2
  integer(kind=nemsio_intkind)                :: version_1     ,version_2
  integer(kind=nemsio_intkind)                :: nmeta_1       ,nmeta_2
  integer(kind=nemsio_intkind)                :: lmeta_1       ,lmeta_2

  ! meta2
  integer(kind=nemsio_intkind)                :: nrec_1        ,nrec_2
  integer(kind=nemsio_intkind)                :: idate_1(7)    ,idate_2(7)
  integer(kind=nemsio_intkind)                :: nfday_1       ,nfday_2
  integer(kind=nemsio_intkind)                :: nfhour_1      ,nfhour_2
  integer(kind=nemsio_intkind)                :: nfminute_1    ,nfminute_2
  integer(kind=nemsio_intkind)                :: nfsecondn_1   ,nfsecondn_2
  integer(kind=nemsio_intkind)                :: nfsecondd_1   ,nfsecondd_2
  integer(kind=nemsio_intkind)                :: dimx_1        ,dimx_2
  integer(kind=nemsio_intkind)                :: dimy_1        ,dimy_2
  integer(kind=nemsio_intkind)                :: dimz_1        ,dimz_2
  integer(kind=nemsio_intkind)                :: nframe_1      ,nframe_2
  integer(kind=nemsio_intkind)                :: nsoil_1       ,nsoil_2
  integer(kind=nemsio_intkind)                :: ntrac_1       ,ntrac_2
  integer(kind=nemsio_intkind)                :: jcap_1        ,jcap_2
  integer(kind=nemsio_intkind)                :: ncldt_1       ,ncldt_2
  integer(kind=nemsio_intkind)                :: idvc_1        ,idvc_2
  integer(kind=nemsio_intkind)                :: idsl_1        ,idsl_2
  integer(kind=nemsio_intkind)                :: idvm_1        ,idvm_2
  integer(kind=nemsio_intkind)                :: idrt_1        ,idrt_2
  real(nemsio_realkind)                       :: rlon_min_1    ,rlon_min_2
  real(nemsio_realkind)                       :: rlon_max_1    ,rlon_max_2
  real(nemsio_realkind)                       :: rlat_min_1    ,rlat_min_2
  real(nemsio_realkind)                       :: rlat_max_1    ,rlat_max_2
  logical(kind=nemsio_logickind)              :: extrameta_1   ,extrameta_2

  ! other standard meta2 records (if nmeta > 2)
  character(len=nemsio_charkind) ,allocatable :: recname(:)        ! nmeta = 3
  character(len=nemsio_charkind) ,allocatable :: reclevtyp(:)      ! nmeta = 4
  integer(kind=nemsio_intkind)   ,allocatable :: reclev(:)         ! nmeta = 5
  real(kind=nemsio_realkind)     ,allocatable :: vcoord(:,:,:)     ! nmeta = 6
  real(kind=nemsio_realkind)     ,allocatable :: lat(:)            ! nmeta = 7
  real(kind=nemsio_realkind)     ,allocatable :: lon(:)            ! nmeta = 8
  real(kind=nemsio_realkind)     ,allocatable :: dx(:)             ! nmeta = 9
  real(kind=nemsio_realkind)     ,allocatable :: dy(:)             ! nmeta = 10
  real(kind=nemsio_realkind)     ,allocatable :: cpi(:)            ! nmeta = 11
  real(kind=nemsio_realkind)     ,allocatable :: ri(:)             ! nmeta = 12

  ! meta3
      ! scalars
  integer(kind=nemsio_intkind)                :: nmetavari_1        ,nmetavari_2
  integer(kind=nemsio_intkind)                :: nmetavarr_1        ,nmetavarr_2
  integer(kind=nemsio_intkind)                :: nmetavarl_1        ,nmetavarl_2
  integer(kind=nemsio_intkind)                :: nmetavarc_1        ,nmetavarc_2
      ! arrays
  integer(kind=nemsio_intkind)                :: nmetaaryi_1        ,nmetaaryi_2
  integer(kind=nemsio_intkind)                :: nmetaaryr_1        ,nmetaaryr_2
  integer(kind=nemsio_intkind)                :: nmetaaryl_1        ,nmetaaryl_2
  integer(kind=nemsio_intkind)                :: nmetaaryc_1        ,nmetaaryc_2

  ! extrameta
  character(nemsio_charkind)     ,allocatable :: variname_1(:)      ,variname_2(:)
  character(nemsio_charkind)     ,allocatable :: varrname_1(:)      ,varrname_2(:)
  character(nemsio_charkind)     ,allocatable :: varlname_1(:)      ,varlname_2(:)
  character(nemsio_charkind)     ,allocatable :: varcname_1(:)      ,varcname_2(:)

  integer(kind=nemsio_intkind)   ,allocatable :: varival_1(:)       ,varival_2(:)
  real(kind=nemsio_realkind)     ,allocatable :: varrval_1(:)       ,varrval_2(:)
  logical(kind=nemsio_logickind) ,allocatable :: varlval_1(:)       ,varlval_2(:)
  character(nemsio_charkind)     ,allocatable :: varcval_1(:)       ,varcval_2(:)

  character(nemsio_charkind)     ,allocatable :: aryiname_1(:)      ,aryiname_2(:)
  character(nemsio_charkind)     ,allocatable :: aryrname_1(:)      ,aryrname_2(:)
  character(nemsio_charkind)     ,allocatable :: arylname_1(:)      ,arylname_2(:)
  character(nemsio_charkind)     ,allocatable :: arycname_1(:)      ,arycname_2(:)

  integer(kind=nemsio_intkind)   ,allocatable :: aryilen_1(:)       ,aryilen_2(:)
  integer(kind=nemsio_intkind)   ,allocatable :: aryrlen_1(:)       ,aryrlen_2(:)
  integer(kind=nemsio_intkind)   ,allocatable :: aryllen_1(:)       ,aryllen_2(:)
  integer(kind=nemsio_intkind)   ,allocatable :: aryclen_1(:)       ,aryclen_2(:)

  integer(kind=nemsio_intkind)   ,allocatable :: aryival_1(:,:)     ,aryival_2(:,:)
  real(kind=nemsio_realkind)     ,allocatable :: aryrval_1(:,:)     ,aryrval_2(:,:)
  logical(kind=nemsio_logickind) ,allocatable :: arylval_1(:,:)     ,arylval_2(:,:)
  character(nemsio_charkind)     ,allocatable :: arycval_1(:,:)     ,arycval_2(:,:)

  character(len=nemsio_charkind) :: vname
  character(len=nemsio_charkind) :: vlevtyp
  integer :: vlev
  integer :: jrec, rec_start, rec_end
  real,allocatable :: data1(:), data2(:)
  integer :: n
  integer :: fieldsize, imin,imax,jmin,jmax

  type(nemsio_gfile) :: gfile1,gfile2

  character(len=256) :: fname1,fname2

  integer :: numarg, i, k
  character(len=256) :: arg, dummy

  integer, external :: iargc

  logical :: verbose = .false.
  logical :: textdump = .false.
  logical :: diff = .false.
  integer :: dump_record = 0
  integer :: idump = -999, jdump = -999
  logical :: ijdump = .false.
  integer :: record = 0
  integer :: idigits
  real*8 :: rms1,rms2,tmp1

  logical :: data1_has_nan, data2_has_nan

!-------------------------------------------------------------------------------

  numarg = command_argument_count()

  if (numarg == 0) call help_info

  i = 0
  do while (i < numarg)
    i = i + 1
    call get_command_argument(i, arg)
    arg = adjustl(arg)

    if (arg(1:1) == "-") then
      if (arg(1:3) == "-h ") then
        call help_info
      else if (arg(1:3) == "-v ") then
        verbose = .true.
      else if (arg(1:5) == "-rec ") then
        i = i + 1
        call get_command_argument(i, dummy)
        read(dummy,*)record
      else if (arg(1:5) == "-bin ") then
        i = i + 1
        call get_command_argument(i, dummy)
        read(dummy,*)dump_record
      else if (arg(1:5) == "-text ") then
        i = i + 1
        call get_command_argument(i, dummy)
        read(dummy,*)dump_record
        textdump = .true.
      else if (arg(1:5) == "-ij ") then
        i = i + 1
        call get_command_argument(i, dummy)
        read(dummy,*) idump,jdump
        ijdump = .true.
      else if (arg(1:6) == "-diff ") then
        i = i + 1
        call get_command_argument(i, fname1)
        i = i + 1
        call get_command_argument(i, fname2)
        diff  = .true.
      else
        write(0,*)" unknown option ",trim(arg)
        call help_info
      end if
    else
      fname1 = arg
    end if
  end do

  call nemsio_init(iret=iret)
  if (iret /= 0) then
    write(0,*)" nemsio_init: iret /= 0 ",iret
    stop
  endif


  call nemsio_open(gfile1,trim(fname1),'READ',iret=iret)
  if (iret /= 0) then
    write(0,*)" error opening file ",trim(fname1)
    write(0,*)" nemsio_open: iret /= 0 ",iret
    stop
  endif

  if (diff) then
  call nemsio_open(gfile2,trim(fname2),'READ',iret=iret)
  if (iret /= 0 ) then
    write(0,*)" error opening file ",trim(fname2)
    write(0,*)" nemsio_open: iret /= 0 ",iret
    stop
  endif
  end if


  ! meta1
  call nemsio_getfilehead(gfile1,iret=iret, &
                          gtype=gtype_1, &
                          gdatatype=gdatatype_1, &
                          modelname=modelname_1, &
                          version=version_1, &
                          nmeta=nmeta_1, &
                          lmeta=lmeta_1)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: (meta1) iret /= 0 ",iret
    stop
  endif

  if (diff) then
  call nemsio_getfilehead(gfile2,iret=iret, &
                          gtype=gtype_2, &
                          gdatatype=gdatatype_2, &
                          modelname=modelname_2, &
                          version=version_2, &
                          nmeta=nmeta_2, &
                          lmeta=lmeta_2)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: (meta1) iret /= 0 ",iret
    stop
  endif
  endif

  if (verbose .or. diff ) then
  print *, ' meta1 '
  print *, ' ------------------------- '

     write(*,"(A,A)",ADVANCE = "NO") ' gtype        ', gtype_1
     if (diff) then
     if (gtype_1 == gtype_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,A,A,A)",ADVANCE = "NO") " *** different *** gtype_1= ",gtype_1, " gtype_2= ",gtype_2
     endif
     endif
     write(*,*)

     write(*,"(A,A)",ADVANCE = "NO") ' gdatatype    ', gdatatype_1
     if (diff) then
     if (gdatatype_1 == gdatatype_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,A,A,A)",ADVANCE = "NO") " *** different *** gdatatype_1= ",gdatatype_1, " gdatatype_2= ",gdatatype_2
     endif
     endif
     write(*,*)

     write(*,"(A,A)",ADVANCE = "NO") ' modelname    ', modelname_1
     if (diff) then
     if (modelname_1 == modelname_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,A,A,A)",ADVANCE = "NO") " *** different *** modelname_1= ",modelname_1, " modelname_2= ",modelname_2
     endif
     endif
     write(*,*)

     write(*,"(A,I7)",ADVANCE = "NO") ' version     ', version_1
     if (diff) then
     if (version_1 == version_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** version_1= ",version_1, " version_2= ",version_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmeta      ', nmeta_1
     if (diff) then
     if (nmeta_1 == nmeta_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmeta_1= ",nmeta_1, " nmeta_2= ",nmeta_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' lmeta      ', lmeta_1
     if (diff) then
     if (lmeta_1 == lmeta_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** lmeta_1= ",lmeta_1, " lmeta_2= ",lmeta_2
     endif
     endif
     write(*,*)
  end if

  ! meta2
  call nemsio_getfilehead(gfile1,iret=iret, &
                          nrec=nrec_1, &
                          idate=idate_1, &
                          dimx=dimx_1, &
                          dimy=dimy_1, &
                          dimz=dimz_1, &
                          nfday=nfday_1, &
                          nfhour=nfhour_1, &
                          nfminute=nfminute_1, &
                          nfsecondn=nfsecondn_1, &
                          nfsecondd=nfsecondd_1, &
                          nframe=nframe_1, &
                          nsoil=nsoil_1, &
                          ntrac=ntrac_1, &
                          jcap=jcap_1, &
                          ncldt=ncldt_1, &
                          idvc=idvc_1, &
                          idsl=idsl_1, &
                          idvm=idvm_1, &
                          idrt=idrt_1, &
                          rlon_min=rlon_min_1, &
                          rlon_max=rlon_max_1, &
                          rlat_min=rlat_min_1, &
                          rlat_max=rlat_max_1, &
                          extrameta=extrameta_1)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: (meta2) iret /= 0 ",iret
    stop
  endif

  if (diff) then
  call nemsio_getfilehead(gfile2,iret=iret, &
                          nrec=nrec_2, &
                          idate=idate_2, &
                          dimx=dimx_2, &
                          dimy=dimy_2, &
                          dimz=dimz_2, &
                          nfday=nfday_2, &
                          nfhour=nfhour_2, &
                          nfminute=nfminute_2, &
                          nfsecondn=nfsecondn_2, &
                          nfsecondd=nfsecondd_2, &
                          nframe=nframe_2, &
                          nsoil=nsoil_2, &
                          ntrac=ntrac_2, &
                          jcap=jcap_2, &
                          ncldt=ncldt_2, &
                          idvc=idvc_2, &
                          idsl=idsl_2, &
                          idvm=idvm_2, &
                          idrt=idrt_2, &
                          rlon_min=rlon_min_2, &
                          rlon_max=rlon_max_2, &
                          rlat_min=rlat_min_2, &
                          rlat_max=rlat_max_2, &
                          extrameta=extrameta_2)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: (meta2) iret /= 0 ",iret
    stop
  endif
  end if

  if (verbose .or. diff ) then
  print *, ' meta2 '
  print *, ' ------------------------- '

     write(*,"(A,I5)",ADVANCE = "NO") ' nrec         ', nrec_1
     if (diff) then
     if (nrec_1 == nrec_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,A,A,A)",ADVANCE = "NO") " *** different *** nrec_1= ",nrec_1, " nrec_2= ",nrec_2
     endif
     endif
     write(*,*)

     write(*,"(A,7(I5))",ADVANCE = "NO") ' idate        ', idate_1
     if (diff) then
!     if (idate_1 == idate_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
!     else
!       write(*,"(A,7(I5),A,7(I5))",ADVANCE = "NO") " *** different *** idate_1= ",idate_1, " idate_2= ",idate_2
!     endif
     endif
     write(*,*)

     write(*,"(A,I7)",ADVANCE = "NO") ' dimx     ', dimx_1
     if (diff) then
     if (dimx_1 == dimx_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** dimx_1= ",dimx_1, " dimx_2= ",dimx_2
     endif
     endif
     write(*,*)

  end if

  imin = 1-nframe_1
  imax = dimx_1+nframe_1
  jmin = 1-nframe_1
  jmax = dimy_1+nframe_1
  fieldsize=(dimx_1+2*nframe_1)*(dimy_1+2*nframe_1)

  ! other standard metadata records
  allocate(recname(nrec_1))
  allocate(reclevtyp(nrec_1))
  allocate(reclev(nrec_1))
  allocate(vcoord(dimz_1+1,3,2))
  allocate(lat(fieldsize))
  allocate(lon(fieldsize))
  allocate(dx(fieldsize))
  allocate(dy(fieldsize))
  allocate(cpi(ntrac_1+1))
  allocate(ri(ntrac_1+1))
  call nemsio_getfilehead(gfile1,iret=iret, &
                          recname=recname, &
                          reclevtyp=reclevtyp, &
                          reclev=reclev, &
                          vcoord=vcoord, &
                          lat=lat, &
                          lon=lon, &
                          dx=dx, &
                          dy=dy, &
                          cpi=cpi, &
                          ri=ri)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: (recname,...) iret /= 0 ",iret
    stop
  endif

  ! user-defined metadata
  call nemsio_getfilehead(gfile1,iret=iret, &

                          nmetavari=nmetavari_1, &
                          nmetavarr=nmetavarr_1, &
                          nmetavarl=nmetavarl_1, &
                          nmetavarc=nmetavarc_1, &

                          nmetaaryi=nmetaaryi_1, &
                          nmetaaryr=nmetaaryr_1, &
                          nmetaaryl=nmetaaryl_1, &
                          nmetaaryc=nmetaaryc_1)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: file1 (nmetavari,...) iret /= 0 ",iret
    stop
  endif


  allocate(variname_1(nmetavari_1))
  allocate(varival_1 (nmetavari_1))

  allocate(varrname_1(nmetavarr_1))
  allocate(varrval_1 (nmetavarr_1))

  allocate(varlname_1(nmetavarl_1))
  allocate(varlval_1 (nmetavarl_1))

  allocate(varcname_1(nmetavarc_1))
  allocate(varcval_1 (nmetavarc_1))

  allocate(aryiname_1(nmetaaryi_1))
  allocate(aryilen_1 (nmetaaryi_1))

  allocate(aryrname_1(nmetaaryr_1))
  allocate(aryrlen_1 (nmetaaryr_1))

  allocate(arylname_1(nmetaaryl_1))
  allocate(aryllen_1 (nmetaaryl_1))

  allocate(arycname_1(nmetaaryc_1))
  allocate(aryclen_1 (nmetaaryc_1))

  call nemsio_getfilehead(gfile1,iret=iret, &
                          variname=variname_1,varival=varival_1, &
                          varrname=varrname_1,varrval=varrval_1, &
                          varlname=varlname_1,varlval=varlval_1, &
                          varcname=varcname_1,varcval=varcval_1, &
                          aryiname=aryiname_1,aryilen=aryilen_1, &
                          aryrname=aryrname_1,aryrlen=aryrlen_1, &
                          arylname=arylname_1,aryllen=aryllen_1, &
                          arycname=arycname_1,aryclen=aryclen_1)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: file1 (variname,...) iret /= 0 ",iret
    stop
  endif

  allocate(aryival_1(maxval(aryilen_1),nmetaaryi_1))
  allocate(aryrval_1(maxval(aryrlen_1),nmetaaryr_1))
  allocate(arylval_1(maxval(aryllen_1),nmetaaryl_1))
  allocate(arycval_1(maxval(aryclen_1),nmetaaryc_1))

  call nemsio_getfilehead(gfile1,iret=iret, &
                          aryival=aryival_1, &
                          aryrval=aryrval_1, &
                          arylval=arylval_1, &
                          arycval=arycval_1)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: file1 (aryival,...) iret /= 0 ",iret
    stop
  endif


! begin reading second nemsio file if -diff option is specified
  if (diff) then

  call nemsio_getfilehead(gfile2,iret=iret, &

                          nmetavari=nmetavari_2, &
                          nmetavarr=nmetavarr_2, &
                          nmetavarl=nmetavarl_2, &
                          nmetavarc=nmetavarc_2, &

                          nmetaaryi=nmetaaryi_2, &
                          nmetaaryr=nmetaaryr_2, &
                          nmetaaryl=nmetaaryl_2, &
                          nmetaaryc=nmetaaryc_2)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: file2 (nmetavari,...) iret /= 0 ",iret
    stop
  endif

  allocate(variname_2(nmetavari_2))
  allocate(varival_2 (nmetavari_2))

  allocate(varrname_2(nmetavarr_2))
  allocate(varrval_2 (nmetavarr_2))

  allocate(varlname_2(nmetavarl_2))
  allocate(varlval_2 (nmetavarl_2))

  allocate(varcname_2(nmetavarc_2))
  allocate(varcval_2 (nmetavarc_2))

  allocate(aryiname_2(nmetaaryi_2))
  allocate(aryilen_2 (nmetaaryi_2))

  allocate(aryrname_2(nmetaaryr_2))
  allocate(aryrlen_2 (nmetaaryr_2))

  allocate(arylname_2(nmetaaryl_2))
  allocate(aryllen_2 (nmetaaryl_2))

  allocate(arycname_2(nmetaaryc_2))
  allocate(aryclen_2 (nmetaaryc_2))

  call nemsio_getfilehead(gfile2,iret=iret, &
                          variname=variname_2,varival=varival_2, &
                          varrname=varrname_2,varrval=varrval_2, &
                          varlname=varlname_2,varlval=varlval_2, &
                          varcname=varcname_2,varcval=varcval_2, &
                          aryiname=aryiname_2,aryilen=aryilen_2, &
                          aryrname=aryrname_2,aryrlen=aryrlen_2, &
                          arylname=arylname_2,aryllen=aryllen_2, &
                          arycname=arycname_2,aryclen=aryclen_2)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: file2 (variname,...) iret /= 0 ",iret
    stop
  endif

  allocate(aryival_2(maxval(aryilen_2),nmetaaryi_2))
  allocate(aryrval_2(maxval(aryrlen_2),nmetaaryr_2))
  allocate(arylval_2(maxval(aryllen_2),nmetaaryl_2))
  allocate(arycval_2(maxval(aryclen_2),nmetaaryc_2))

  call nemsio_getfilehead(gfile2,iret=iret, &
                          aryival=aryival_2, &
                          aryrval=aryrval_2, &
                          arylval=arylval_2, &
                          arycval=arycval_2)
  if (iret /= 0) then
    write(0,*)" nemsio_getfilehead: file2 (aryival,...) iret /= 0 ",iret
    stop
  endif

  end if ! diff
! end of reading second nemsio file


  if (verbose) then

  print *, ' meta2 '
  print *, ' ------------------------- '
  print *, ' nrec         = ', nrec_1
  print *, ' idate        = ', idate_1
  print *, ' dimx         = ', dimx_1
  print *, ' dimy         = ', dimy_1
  print *, ' dimz         = ', dimz_1
  print *, ' nfday        = ', nfday_1
  print *, ' nfhour       = ', nfhour_1
  print *, ' nfminute     = ', nfminute_1
  print *, ' nfsecondn    = ', nfsecondn_1
  print *, ' nfsecondd    = ', nfsecondd_1
  print *, ' nframe       = ', nframe_1
  print *, ' nsoil        = ', nsoil_1
  print *, ' ntrac        = ', ntrac_1
  print *, ' jcap         = ', jcap_1
  print *, ' ncldt        = ', ncldt_1
  print *, ' idvc         = ', idvc_1
  print *, ' idsl         = ', idsl_1
  print *, ' idvm         = ', idvm_1
  print *, ' idrt         = ', idrt_1
  print *, ' rlon_min     = ', rlon_min_1
  print *, ' rlon_max     = ', rlon_max_1
  print *, ' rlat_min     = ', rlat_min_1
  print *, ' rlat_max     = ', rlat_max_1
  print *, ' extrameta    = ', extrameta_1
  print *, ' '

   print *, ' meta3-meta12 iret = ', iret
   print *, ' ------------------------- '
   !do n=1,nrec_1
   !print *, ' recname, reclevtyp, reclev ',recname(n), reclevtyp(n), reclev(n)
   !end do
   print *,'vcoord(:,1,1)=',vcoord(:,1,1)
   print *,'vcoord(:,2,1)=',vcoord(:,2,1)
   print *,'vcoord(:,3,1)=',vcoord(:,3,1)
   print *,'vcoord(:,1,2)=',vcoord(:,1,2)
   print *,'vcoord(:,2,2)=',vcoord(:,2,2)
   print *,'vcoord(:,3,2)=',vcoord(:,3,2)
   print *,'lat=',maxval(lat),minval(lat)
   print *,'lon=',maxval(lon),minval(lon)
   print *,'dx=',maxval(dx),minval(dx)
   print *,'dy=',maxval(dy),minval(dy)

  print *, ' meta3 '
  print *, ' ------------------------- '

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetavari    ', nmetavari_1
     if (diff) then
     if (nmetavari_1 == nmetavari_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetavari_1= ",nmetavari_1, " nmetavari_2= ",nmetavari_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetavarr    ', nmetavarr_1
     if (diff) then
     if (nmetavarr_1 == nmetavarr_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetavarr_1= ",nmetavarr_1, " nmetavarr_2= ",nmetavarr_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetavarl    ', nmetavarl_1
     if (diff) then
     if (nmetavarl_1 == nmetavarl_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetavarl_1= ",nmetavarl_1, " nmetavarl_2= ",nmetavarl_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetavarc    ', nmetavarc_1
     if (diff) then
     if (nmetavarc_1 == nmetavarc_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetavarc_1= ",nmetavarc_1, " nmetavarc_2= ",nmetavarc_2
     endif
     endif
     write(*,*)



     write(*,"(A,I5)",ADVANCE = "NO") ' nmetaaryi    ', nmetaaryi_1
     if (diff) then
     if (nmetaaryi_1 == nmetaaryi_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetaaryi_1= ",nmetaaryi_1, " nmetaaryi_2= ",nmetaaryi_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetaaryr    ', nmetaaryr_1
     if (diff) then
     if (nmetaaryr_1 == nmetaaryr_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetaaryr_1= ",nmetaaryr_1, " nmetaaryr_2= ",nmetaaryr_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetaaryl    ', nmetaaryl_1
     if (diff) then
     if (nmetaaryl_1 == nmetaaryl_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetaaryl_1= ",nmetaaryl_1, " nmetaaryl_2= ",nmetaaryl_2
     endif
     endif
     write(*,*)

     write(*,"(A,I5)",ADVANCE = "NO") ' nmetaaryc    ', nmetaaryc_1
     if (diff) then
     if (nmetaaryc_1 == nmetaaryc_2) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A,I7,A,I7)",ADVANCE = "NO") " *** different *** nmetaaryc_1= ",nmetaaryc_1, " nmetaaryc_2= ",nmetaaryc_2
     endif
     endif
     write(*,*)


  do n=1,nmetavari_1
    write(*,"(A,A,I14)",ADVANCE = "NO") 'variname,varival=',variname_1(n),varival_1(n)
    if (diff) then
     if ( varival_1(n) == varival_2(n) ) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A)",ADVANCE = "NO") " *** different *** "
     endif
    endif
    write(*,*)
  end do
  do n=1,nmetavarr_1
    write(*,"(A,A,E16.9)",ADVANCE = "NO") 'varrname,varrval=',varrname_1(n),varrval_1(n)
    if (diff) then
     if ( varrval_1(n) == varrval_2(n) ) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A)",ADVANCE = "NO") " *** different *** "
     endif
    endif
    write(*,*)
  end do
  do n=1,nmetavarl_1
    write(*,"(A,A,L8)",ADVANCE = "NO") 'varlname,varlval=',varlname_1(n),varlval_1(n)
    if (diff) then
     if ( varlval_1(n) .eqv. varlval_2(n) ) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A)",ADVANCE = "NO") " *** different *** "
     endif
    endif
    write(*,*)
  end do
  do n=1,nmetavarc_1
    write(*,"(A,A,A)",ADVANCE = "NO") 'varcname,varcval=',varcname_1(n),varcval_1(n)
    if (diff) then
     if ( varcval_1(n) == varcval_2(n) ) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A)",ADVANCE = "NO") " *** different *** "
     endif
    endif
    write(*,*)
  end do


  do n=1,nmetaaryi_1
    write(*,"(A,A,I8,I14,A,I14)",ADVANCE = "NO") ' aryiname, aryilen, aryival ', aryiname_1(n),aryilen_1(n),aryival_1(1,n),' ... ',aryival_1(aryilen_1(n),n)
    if (diff) then
     if ( minval(aryival_1(:,n)-aryival_2(:,n))==0.0 .and.  maxval(aryival_1(:,n)-aryival_2(:,n))==0.0 ) then
       write(*,"(A)",ADVANCE = "NO") " OK"
     else
       write(*,"(A)",ADVANCE = "NO") " *** different *** "
     endif
    endif
    write(*,*)
  end do
  do n=1,nmetaaryr_1
    write(*,"(A,A,I8,E14.7,A,E14.7)",ADVANCE = "NO") ' aryrname, aryrlen, aryrval ', aryrname_1(n),aryrlen_1(n),aryrval_1(1,n),' ... ',aryrval_1(aryrlen_1(n),n)
    if (diff) then
      if ( minval(aryrval_1(:,n)-aryrval_2(:,n))==0.0 .and. maxval(aryrval_1(:,n)-aryrval_2(:,n))==0.0 ) then
        write(*,"(A)",ADVANCE = "NO") " OK"
     else
        write(*,"(A)",ADVANCE = "NO") " *** different ***"
     endif
    endif
    write(*,*)
  end do
  do n=1,nmetaaryl_1
    write(*,"(A,A,I8,L4,A,L4)",ADVANCE = "NO") ' arylname, aryllen, arylval ', arylname_1(n),aryllen_1(n),arylval_1(1,n),' ... ',arylval_1(aryllen_1(n),n)
!    if (diff) then
!      if ( arylval_1(:,n) .eqv. arylval_2(:,n) ) then
!        write(*,"(A)",ADVANCE = "NO") " OK"
!      else
!        write(*,"(A)",ADVANCE = "NO") " *** different *** "
!      endif
!    end if
    write(*,*)
  end do

  end if

  allocate(data1(fieldsize))
  allocate(data2(fieldsize))

  if (dump_record>0) then

     call nemsio_readrec(gfile1,dump_record,data1,nframe=nframe_1,iret=iret)
     if (iret /= 0) then
       write(0,*)" nemsio_readrec: file1 (dump_record,...) iret /= 0 ",iret
       stop
     endif

     if (textdump) then
        open(unit=99,file="dump",form="formatted",status="unknown")
        do k=1,fieldsize
        write(99,'(I7,E16.9)')k,data1(k)
        end do
        close(unit=99)
     else
        open(unit=99,file="dump",form="unformatted",status="unknown")
        write(99)data1
        close(unit=99)
     end if

  else

     if (record > 0) then
        if (record > nrec_1) then
           write(0,*)' There are only ',nrec_1,' records. You asked for record ',record
           stop
        end if
        rec_start = record
        rec_end = record
     else
        rec_start = 1
        rec_end = nrec_1
     end if

     do jrec=rec_start, rec_end

        call nemsio_getrechead(gfile1,jrec,vname,vlevtyp,vlev,iret=iret)
        if (iret /= 0) then
          write(0,*)" nemsio_getrechead: file1 (jrec,...) iret /= 0 ",iret
          stop
        endif

        if ( (verbose .or. ijdump ) .and. .not.diff) then
          call nemsio_readrec(gfile1,jrec,data1,nframe=nframe_1,iret=iret)
          if (iret /= 0) then
            write(0,*)" nemsio_readrec: file1 (jrec,...) iret /= 0 ",iret
            stop
          endif
          if (ijdump) then
             if (idump<imin.or.idump>imax.or.jdump<jmin.or.jdump>jmax) then
                write(0,*)" I,J outside the domain i=",idump," jdump=",jdump
                stop
             end if
             k = (imax-imin+1)*(jdump-jmin)+idump+nframe_1
             if (k<1.or.k>fieldsize) then
                write(0,*)" k<1 or k>fieldsize k=",k," fieldsize=",fieldsize
                stop
             end if
             write(*,"(I5,1X,A,1X,A,1X,I4,1X,A,I4,A,I4,1X,E16.9)")jrec,vname,vlevtyp,vlev,' i=',idump,' j=',jdump,data1(k)
          else
             data1_has_nan = .false.
             do k=1,fieldsize
               if ( data1(k)/=data1(k) ) then
                  data1_has_nan =.true.
                  exit
               end if
             end do
             if ( data1_has_nan ) then
             write(*,"(I5,1X,A,1X,A,1X,I4,2(A,E16.9),A)")jrec,vname,vlevtyp,vlev,' minval=',minval(data1),' maxval=',maxval(data1),' data1_has_nan '
             else
             write(*,"(I5,1X,A,1X,A,1X,I4,2(A,E16.9)  )")jrec,vname,vlevtyp,vlev,' minval=',minval(data1),' maxval=',maxval(data1)
             endif
          end if
        else if (diff) then
           call nemsio_readrec(gfile1,jrec,data1,nframe=nframe_1,iret=iret)
           call nemsio_readrecv(gfile2,vname,vlevtyp,vlev,data2,nframe=nframe_2,iret=iret)
           if (iret /= 0) then
             write(*,"(I5,1X,A,1X,A,1X,I4,2(A,E14.7))")jrec,vname,vlevtyp,vlev,' MISSING'
             cycle
           endif

           !write(*,"(I5,1X,A,1X,A,1X,I4,2(A,E14.7))",ADVANCE = "NO")jrec,vname,vlevtyp,vlev,' min(diff)=',minval(data1-data2),' max(diff)=',maxval(data1-data2)
           write(*,"(I5,1X,A,1X,A,1X,I4)",ADVANCE = "NO")jrec,vname,vlevtyp,vlev
           ! this is slow... check if there are any NaNs
           data1_has_nan = .false.
           data2_has_nan = .false.
           do k=1,fieldsize
             if ( data1(k)/=data1(k) ) then
                data1_has_nan =.true.
                exit
             end if
           end do
           do k=1,fieldsize
             if ( data2(k)/=data2(k) ) then
                data2_has_nan =.true.
                exit
             end if
           end do
           if ( data1_has_nan .or. data2_has_nan ) then
               write(*,"(A)",ADVANCE = "NO") " data1_has_nan .or. data2_has_nan "
           end if
           if (minval(data1-data2)==0.0 .and. maxval(data1-data2)==0.0) then
             write(*,"(A)") " OK"
           else
     !        write(*,"(A,I7,A,I7)") " *** different *** minloc= ",MINLOC(data1-data2), " maxloc= ",MAXLOC(data1-data2)

             rms1=sqrt(sum(data1*data1)/dble(size(data1)))
             rms2=sqrt(sum(data2*data2)/dble(size(data2)))
             idigits = 0
             if ( rms1-rms2 .EQ. 0.0d0 ) then
               idigits = 15
             else
               if ( rms2 .ne. 0 ) then
                 tmp1 = 1.0d0/((abs(rms1-rms2))/rms2)
                 if (tmp1 .NE. 0 ) then
                   !idigits=max(0.0d0,log10(tmp1))  ! can go negative if the exponents don't even agree
                   idigits=log10(tmp1)   !let it go that way
                 endif
               endif
             endif
             write(*,"(4(A,e14.7),A,I4,2(A,I7))")' min(diff)=',minval(data1-data2),' max(diff)=',maxval(data1-data2), &
                     ' rms1=',rms1,' rms2=',rms2,                                                                 &
                     ' digits=',idigits,' minloc=',MINLOC(data1-data2),' maxloc=',MAXLOC(data1-data2)
           endif
        else
          write(*,"(I5,1X,A,1X,A,1X,I4)")jrec,vname,vlevtyp,vlev
        end if

     enddo

  end if

  call nemsio_close(gfile1,iret=iret)
  if (iret /= 0) then
    write(0,*)" nemsio_close: file1 iret /= 0 ",iret
    stop
  endif

  if (diff) then
  call nemsio_close(gfile2,iret=iret)
  if (iret /= 0) then
    write(0,*)" nemsio_close: file2 iret /= 0 ",iret
    stop
  endif
  end if

  call nemsio_finalize()

  stop

contains

  subroutine help_info

  write(0,*) " "
  write(0,*) " usage: dnemsio [option] nemsio_file1 [nemsio_file2] "
  write(0,*) " "
  write(0,*) " Options available are:"
  write(0,*) "    -help                  : print this information"
  write(0,*) "    -v                     : verbose nemsio file dump"
  write(0,*) "    -rec rec_number        : dump record rec_number"
  write(0,*) "    -bin rec_number        : dump record rec_number in binary file"
  write(0,*) "    -text rec_number       : dump record rec_number in text file"
  write(0,*) "    -ij I,J                : print values and I,J"
  write(0,*) "    -diff                  : find the difference between file1 and file2"
  write(0,*) " "
  write(0,*) "    nemsio_file1           : the nemsio file"
  write(0,*) "    nemsio_file2           : the second nemsio file; used when -diff option is used"
  write(0,*) " "

  stop

  end subroutine help_info

end program dnemsio
