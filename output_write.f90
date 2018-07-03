!    %-----------------------------------------------------------------%
!    |  D P D  --  version 1.4    /    December  2008  --   y.afshar   |
!    %-----------------------------------------------------------------%

!    %------------------ THIS IS OUTPUT_WRITE ---------------------%
!    |                                                             |
!    | Subroutine corout - write out correlation data file         |
!    |                                                             |
!    | Subroutine conout - making output configuartion file        |
!    |                                                             |
!    %-------------------------------------------------------------%

Subroutine corout(time)

!    %--------------------------------------------------------------%
!    |  routine to write out correlation data file                  |
!    %--------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use statistics_module, Only : stpval

  Implicit None

  Real( Kind = wp ),  Intent( In    ) :: time
  Logical,                       Save :: newjob
  Data                                   newjob / .true. /

  If (newjob) Then
     newjob = .false.
     Open(ncorr, File='PLOT.plt')
     Write(ncorr,'("Variables=",6x,"time,",7x,"en-total,",5x,"pe-total,",5x,   &
                                        "pressure,",4x,"temperature")')
  End If

! write out data
  Write(ncorr,'(8x,f12.4,3x,f13.6,1x,f13.6,1x,f13.6,2x,f13.6)')                &
                time,stpval(4),stpval(1),stpval(5),stpval(6)

End Subroutine corout

!***********************************************************************

Subroutine conout(nstep,time)

!    %--------------------------------------------------------------%
!    |  subroutine for making plot of output configuartion file     |
!    %--------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : natms,nlast,celx,cely,celz,numtyp,ltype,           &
                            xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz
  Use param_module

  Implicit None

  Integer,           Intent( In    ) :: nstep
  Real( Kind = wp ), Intent( In    ) :: time

  Character( Len = 13 ) :: name
  Character( Len = 7  ) :: fname
  Character( Len = 6  ) :: Title
  Character( Len = 17 ) :: filname
  Character( Len = 8 )  :: color

  Integer               :: i,j,index
  Real( kind = wp )     :: xyz(1:6)

  fname='OUTCONF'
  color='CUSTOM'

  If (nstep==0) Then
     Call getfn(fname,0,1,name)
     filname=name(1:13)//'.plt'
     Open(ncout, File = filname)
     Goto 1
  End If

  If (.not.(nstep >= nstout .and. Mod(nstep-nstout, istout) == 0)) Return

  index=(nstep-nstout)/istout+1

  Call getfn(fname,index,1,name)

  filname=name(1:13)//'.plt'
  Open(ncout, File = filname, Status = 'Replace')

1 Write(ncout,2) time,nstep
2 Format('TITLE = "Configuration at time =',e15.8,', at step =',i10,'"')

  Write(ncout,*) 'Variables= "X","Y","Z","Vx","Vy","Vz"'

  Do j=1,mxatyp

     Select Case (j)

        Case (1)

! New title for zone that include different atom type

           Write(Title(1:6),'(a,i2)')'Type',j
           Write(color(7:8),'(2i1)')Mod(j/10, 10), Mod(j, 10)+1
           Write(ncout,'(3a, i10, 3a, e15.8)') 'Zone T="',Title,'" i=',Nint(numtyp(j)),'F=Point,C=',color,', SOLUTIONTIME=',time  !C=Blue'

           Do i=1,natms

              If (ltype(i) == j) Write(ncout,'(6g20.10)') xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i)

           End Do

        Case Default

! New title for zone that include different atom type

           Write(Title(1:6),'(a,i2)')'Type',j
           Write(color(7:8),'(2i1)')Mod(j/10, 10), Mod(j, 10)+1
           Write(ncout,'(3a, i10, 3a, e15.8)') 'Zone T="',Title,'" i=',Nint(numtyp(j)),'F=Point,C=',color,', SOLUTIONTIME=',time

           Do i=1,natms

              If (ltype(i) == j) Write(ncout,'(6g20.10)') xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i)

           End Do

        End Select

  End Do

  If (nlast /= natms) Then

     Write(color(7:8),'(2i1)')Mod(j/10, 10), Mod(j, 10)+1
     Write(ncout,'(a, i10, 3a, e15.8)') 'Zone T= halo  i=',(nlast-natms),'F=Point,C=',color,', SOLUTIONTIME=',time

     Do i=natms+1,nlast
        Write(ncout,'(6g20.10)') xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i)
     End Do

  End If

  xyz(1) = -celx/2.0_wp
  xyz(2) = -cely/2.0_wp
  xyz(3) = -celz/2.0_wp
  xyz(4) = -xyz(1)
  xyz(5) = -xyz(2)
  xyz(6) = -xyz(3)

  Write(ncout,Fmt='(a, e15.8)')'Zone I=16 F=Point,C=Black,SOLUTIONTIME=', time
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(2),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(2),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(5),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(5),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(2),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(2),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(2),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(2),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(2),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(5),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(5),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(4),xyz(5),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(5),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(5),xyz(3),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(5),xyz(6),0,0,0
  Write(ncout,'(3g20.10, 3i3)')xyz(1),xyz(2),xyz(6),0,0,0

  Close(Unit=ncout)

End Subroutine conout

!***********************************************************************

Subroutine vd_profile(yesornot,ns,ns2)

!    %--------------------------------------------------------------%
!    |  subroutine for making plot of velocity profile              |
!    %--------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : natms,celx,cely,celz,volm,                         &
                            xxx,yyy,zzz,vxx,vyy,vzz

  Use param_module, Only : rbin

  Implicit None

  Integer,              Intent( In    ) :: yesornot,ns,ns2

  Logical,                         Save :: newjob
  Data                                     newjob / .true. /

  Integer                               :: n,i,j,k,index,nlx,nly,nlz,fail(1:10)
  Integer,                         Save :: nacc
  Real ( kind = wp )                    :: rbin0,dx0,dy0,dz0,ubar

  Real ( kind = wp ), Allocatable, Save :: uuu (:), www(:) !, vvv(:)
  Real ( kind = wp ), Allocatable, Save :: uux (:), uuz(:)
  Real ( kind = wp ), Allocatable, Save :: vvx (:), vvz(:)
! ! ! !   Real ( kind = wp ), Allocatable, Save :: rhox(:)

  Integer,            Allocatable, Save :: nacc0(:), nacc1(:), nacc2(:)

  Character( Len = 22 ) :: name
  Character( Len = 16 ) :: fname1
  Character( Len = 15 ) :: fname2
  Character( Len = 14 ) :: fname3
  Character( Len = 12 ) :: fname4

  Character( Len = 26 ) :: filname1
  Character( Len = 25 ) :: filname2
  Character( Len = 24 ) :: filname3
  Character( Len = 22 ) :: filname4

  rbin0 = 4.0_wp*rbin
  fail = 0

  nlx = Int(celx/rbin0)
  nly = Int(cely/rbin0)
  nlz = Int(celz/rbin0)

  If (Iand(nlx,nlx-1) /= 0) nlx=nlx+1

  dx0 = celx/Real(nlx,wp)
  dy0 = cely/Real(nly,wp)
  dz0 = celz/Real(nlz,wp)

  index = (nlx+3)*(nlz+3)

  If (newjob) Then
     newjob = .false.

     Allocate (uuu(1:index),www(1:index), Stat=fail(1))
! ! ! !      Allocate (rhox(1:nlx+3),             Stat=fail(2))
     Allocate (uux(1:nlx+3),uuz(1:nlx+3), Stat=fail(3))
     Allocate (vvx(1:nlz+3),vvz(1:nlz+3), Stat=fail(4))
     Allocate (nacc0(1:index),            Stat=fail(5))
     Allocate (nacc1(1:nlx+3),            Stat=fail(6))
     Allocate (nacc2(1:nlz+3),            Stat=fail(7))

     If (Any(fail > 0)) Then
         Write(nrite,'(/,1x,a)') 'velocity profile allocation failure'
         Call error(0)
     End If

     uuu = 0.0_wp
     www = 0.0_wp

! ! ! !      rhox = 0.0_wp

     uux = 0.0_wp
     uuz = 0.0_wp

     vvx = 0.0_wp
     vvz = 0.0_wp

     nacc = 0
     nacc0 = 0
     nacc1 = 0
     nacc2 = 0

  End If

  nacc  = nacc + 1

  Do n = 1 , natms

     i = Int((xxx(n)+celx/2.0_wp+1.5_wp*dx0)/dx0)
!    j = Int((yyy(n)+cely/2.0_wp+1.5_wp*dy0)/dy0)+1
     k = Int((zzz(n)+celz/2.0_wp+1.5_wp*dz0)/dz0)

     index = 1 + i + k*(nlx+3)

!      If (yyy(n) >= -10._wp .and. yyy(n) <= 10._wp) Then
        uuu(index) = uuu(index) + vxx(n)
         !vvv(index) = vvv(index) + vyy(n)
        www(index) = www(index) + vzz(n)
        nacc0(index) = nacc0(index) + 1
!      End If

! ! ! !      If (zzz(n) >= -rbin0 .and. zzz(n) <= rbin0) Then
        index = 1 + i
        uux(index) = uux(index) + vxx(n)
        uuz(index) = uuz(index) + vzz(n)
        nacc1(index) = nacc1(index) + 1
! ! ! !      End If

!      If (xxx(n) >= -rbin0 .and. xxx(n) <= rbin0) Then
        index = 1 + k
        vvx(index) = vvx(index) + vxx(n)
        vvz(index) = vvz(index) + vzz(n)
        nacc2(index) = nacc2(index) + 1
!      End If

! ! ! !      index = 1 + i
! ! ! !      rhox(index) = rhox(index) + 1.0_wp

  End Do

  If (yesornot == 1) Goto 1

  Return

1 fname1='Velocity.Profile'
  index=ns/ns2
  Call getfn(fname1,index,1,name)
  filname1=name(1:22)//'.plt'

  Open(Unit=nvout, File=filname1, Status='new')

! ! ! !   fname2='Density.Profile'
! ! ! !   Call getfn(fname2,index,1,name)
! ! ! !   filname2=name(1:21)//'.plt'
! ! ! !
! ! ! !   Open(Unit=ndout, File=filname2, Status='new')

  fname3='Horizontal.Cut'
  Call getfn(fname3,index,1,name)
  filname3=name(1:20)//'.plt'

  Open(Unit=nhout,File=filname3, Status='new')

  fname4='Vertical.Cut'
  Call getfn(fname4,index,1,name)
  filname4=name(1:18)//'.plt'

  Open(Unit=nverout, File=filname4, Status='new')

  Write(nvout,*) 'Variables = "x" , "y" , "u" , "w" '
  Write(nvout,*) 'Zone T= velocity  i=',nlx+1,'k=',nlz+1,'F=Point,C=Red'

  Do k = 1,nlz+1
     Do i = 1,nlx+1
        index = 1 + i + k*(nlx+3)

        If (nacc0(index) == 0) nacc0(index) = 1

        Write(nvout,*) Real(-celx/2.0_wp-1.5_wp*dx0+i*dx0+dx0/2.0_wp , wp),    &
                       Real(-celz/2.0_wp-1.5_wp*dz0+k*dz0+dz0/2.0_wp , wp),    &
                       uuu(index)/Real(nacc0(index),wp),                       &
                       www(index)/Real(nacc0(index),wp)
     End Do
  End do

  Write(nrite,'(/,1x,a,i10,a)') 'averages of velocity profiles calculated over',nacc,' steps'
  Close(Unit=nvout)

! ! ! !   Write(ndout,*) 'Variables = "x" , "density" '
! ! ! !   Write(ndout,*) 'Zone T= Density  i=',nlx+1, 'F=Point,C=Red'
! ! ! !
! ! ! !
! ! ! !   Write(ndout,*) Real(-celx/2.0_wp-1.5_wp*dx0+dx0+dx0/2.0_wp , wp),            &
! ! ! !                  (rhox(2)+rhox(nlx+2))/(Real(nacc,wp)*(volm/Real(nlx,wp)))
! ! ! !
! ! ! !   Do i = 3,nlx+1
! ! ! !
! ! ! !      Write(ndout,*) Real(-celx/2.0_wp-1.5_wp*dx0+(i-1)*dx0+dx0/2.0_wp , wp),   &
! ! ! !                     rhox(i)/(Real(nacc,wp)*(volm/Real(nlx,wp)))
! ! ! !   End Do
! ! ! !
! ! ! !   Write(ndout,*) Real(-celx/2.0_wp-1.5_wp*dx0+(nlx+1)*dx0+dx0/2.0_wp , wp),    &
! ! ! !                  (rhox(2)+rhox(nlx+2))/(Real(nacc,wp)*(volm/Real(nlx,wp)))
! ! ! !
! ! ! !
! ! ! !   Write(nrite,'(/,1x,a,i10,a)') 'average of density calculated over',nacc,' steps'
! ! ! !   Close(Unit=ndout)

  Write(nhout,*) 'Variables = "x" , "vx" , "vz"'
  Write(nhout,*) 'Zone T= velocity  i=',nlx+1, 'F=Point,C=Black'

  ubar=0.0_wp

  Do i = 2,nlx+2

     If (nacc1(i) == 0) nacc1(i) = 1
     Write(nhout,*) Real(-celx/2.0_wp-1.5_wp*dx0+(i-1)*dx0+dx0/2.0_wp , wp),   &
                    uux(i)/Real(nacc1(i),wp),uuz(i)/Real(nacc1(i),wp)

!      If (Iand(nlx+1, nlx) == 0 ) Then
        If (i > (nlx+3)/2 .and. i < nlx+2) Then
           ubar=ubar+uuz(i)/Real(nacc1(i),wp)
        Else If (i == nlx+2) Then
           ubar=ubar+0.5_wp*uuz(i)/Real(nacc1(i),wp)
        End If
!      End If
  End Do
  ubar=ubar/(0.5_wp*(nlx+1))
  Write(nrite,'(/,1x,a,i10,a,i10)') 'average of horizontal velocity calculated over',nacc,' steps',nlx
  Write(nrite,'(/,1x,a,i10,a,1p,e14.6)') 'average of horizontal velocity calculated over',nacc,' steps that is',ubar

  Write(nverout,*) 'Variables = "z" , "vx" , "vz"'
  Write(nverout,*) 'Zone T= velocity  i=',nlz+1, 'F=Point,C=Black'

  Do i = 2,nlz+2

     If (nacc2(i) == 0) nacc2(i) = 1
     Write(nverout,*) Real(-celz/2.0_wp-1.5_wp*dz0+(i-1)*dz0+dz0/2.0_wp , wp), &
                      vvx(i)/Real(nacc2(i),wp),vvz(i)/Real(nacc2(i),wp)

  End Do

  Write(nrite,'(/,1x,a,i10,a,/,/)') 'average of vertical velocity calculated over',nacc,' steps'

  newjob = .true.

  Deallocate(uuu,www, Stat=fail(1))
! ! ! !   Deallocate(rhox,    Stat=fail(2))
  Deallocate(uux,uuz, Stat=fail(3))
  Deallocate(vvx,vvz, Stat=fail(4))
  Deallocate(nacc0,   Stat=fail(5))
  Deallocate(nacc1,   Stat=fail(6))
  Deallocate(nacc2,   Stat=fail(7))

  If (Any(fail > 0)) Then
      Write(nrite,'(/,1x,a)') 'velocity profile deallocation failure'
      Call error(0)
  End If

End Subroutine vd_profile

!***********************************************************************