!    %-----------------------------------------------------------------%
!    |  D P D  --  version 1.4    /    December  2008  --   y.afshar   |
!    %-----------------------------------------------------------------%

!    %----------------------- THIS IS INPUT_ROUTINES --------------------------%
!    |                                                                         |
!    |   Subroutines for loading or generating initial particle information    |
!    |                                                                         |
!    | Subroutine input - read the initial configuration from file             |
!    |                                                                         |
!    | Subroutine Orthorhombic                                                 |
!    |                                                                         |
!    | Subroutine cavity                                                       |
!    |                                                                         |
!    %-------------------------------------------------------------------------%

Subroutine input

  Use kinds_f90
  Use setup_module
  Use config_module, Only : natms,nlast,weight,ltype,lstfrz,                   &
                            xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz,               &
                            celx,cely,celz

  Implicit None

  Integer             :: i,j,k
  Real( Kind = wp )   :: xyz(1:3)

! READ FROM TRAJECTORY_ROUTINE OUTPUT FILE

!  Do i = 1,nlast
!  !Do i = 1,natms
!
!     !Write(Unit=nread, Fmt='(f8.4,i2,1p,7e13.5)', Rec=rec) weight(i),       &
!	 !     ltype(i),rsd(i),xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i),lf
!     Read(nread, Fmt='(2f12.6,2i4)') weight(i),xyz(1),ltype(i),lstfrz(i)
!     Read(nread, Fmt='(1p,3e12.4)') xxx(i),yyy(i),zzz(i)
!     Read(nread, Fmt='(1p,3e12.4)') vxx(i),vyy(i),vzz(i)
!     Read(nread, Fmt='(1p,3e12.4)') fxx(i),fyy(i),fzz(i)
!
!  End Do

! READ FROM WRITE_CONFIG OUTPUT FILE

  Do i=1,nlast
!  Do i=1,natms

     Read(Unit=nread, Fmt='(g20.10,2i10)') weight(i),ltype(i),lstfrz(i)
     Read(Unit=nread, Fmt='(3g20.10)') xxx(i),yyy(i),zzz(i)
     Read(Unit=nread, Fmt='(3g20.10)') vxx(i),vyy(i),vzz(i)
!     Write(Unit=nread, Fmt='(3g20.10)') fxx(i),fyy(i),fzz(i)

  End Do

  Close(Unit=nread)

End Subroutine input

!***********************************************************************

Subroutine Orthorhombic

!    %--------------------------------------------------------------%
!    |  Subroutines for generating initial particle information     |
!    |  for Orthorhombic periodic boundaries geometry               |
!    %--------------------------------------------------------------%


  Use kinds_f90
  Use param_module,    Only : natms_x,natms_y,natms_z
  Use config_module
  Use random_module

  Implicit None

  Real( Kind = wp )         :: dx,dy,dz
  Integer                   :: i,j,k,l

  dx=celx/Real(natms_x,wp)
  dy=cely/Real(natms_y,wp)
  dz=celz/Real(natms_z,wp)

! Note the origin of the coordinates is the centre of the cell

  l=1

  Do i=1,natms_x
     Do j=1,natms_y
        Do k=1,natms_z

           xxx(l)=(i-1)*dx+dx/2.0_wp-celx/2.0_wp
           yyy(l)=(j-1)*dy+dy/2.0_wp-cely/2.0_wp
           zzz(l)=(k-1)*dz+dz/2.0_wp-celz/2.0_wp
           ltype(l)=1

! set initial velocities & weight & lstfrz

           vxx(l)=kgauss()
           vyy(l)=kgauss()
           vzz(l)=kgauss()

           weight(l)=1.0_wp
           lstfrz(l)=0

           If (lstfrz(l) /= 0) Then

              vxx(l)=0.0_wp
              vyy(l)=0.0_wp
              vzz(l)=0.0_wp

           End If

           l=l+1

        End Do
     End Do
  End Do

  natms = l-1
  nlast = natms

End Subroutine

!***********************************************************************

Subroutine Orthorhombic2

  !    %--------------------------------------------------------------%
  !    |  Subroutines for generating initial particle information     |
  !    |  in randomly order for Orthorhombic periodic boundaries      |
  !    |  geometry                                                    |
  !    %--------------------------------------------------------------%

  Use kinds_f90
  Use param_module,    Only : shrate, imcon, natms_x,natms_y,natms_z
  Use config_module
  Use random_module

  Implicit None

  Integer                   :: i,j,k,l

! Note the origin of the coordinates is the centre of the cell

  l=1

  Do i=1,natms_x
     Do j=1,natms_y
        Do k=1,natms_z

           xxx(l)=celx*(unif()-0.5_wp)
           yyy(l)=cely*(unif()-0.5_wp)
           zzz(l)=celz*(unif()-0.5_wp)

           ltype(l)=1

! set initial velocities & weight & lstfrz
           vxx(l)=kgauss()
           vyy(l)=kgauss()
           vzz(l)=kgauss()
           If (imcon == 4) vxx(l)=shrate*zzz(l)

           weight(l)=1.0_wp
           lstfrz(l)=0

           If (lstfrz(l) /= 0) Then

              vxx(l)=0.0_wp
              vyy(l)=0.0_wp
              vzz(l)=0.0_wp

           End If

           l=l+1

        End Do
     End Do
  End Do

  natms = l-1
  nlast = natms

End Subroutine

!***********************************************************************

Subroutine cavity

!    %--------------------------------------------------------------%
!    |  Subroutines for generating initial particle information     |
!    |  for Shear cavity geometry                                   |
!    %--------------------------------------------------------------%

  Use kinds_f90
  Use param_module,    Only : natms_x,natms_y,natms_z,                         &
                              vatms_x,vatms_y,vatms_z
  Use config_module
  Use setup_module

  Implicit None

  Real( Kind = wp )         :: dx,dy,dz
  Integer                   :: i,j,k,l,m
  Logical                   :: safe

  dx=celx/Real((natms_x),wp)
  dy=cely/Real((natms_y),wp)
  dz=celz/Real((natms_z),wp)

! Note the origin of the coordinates is the centre of the cell

  l=1

  Do i=1,natms_x
     Do j=1,natms_y
        Do k=1,natms_z

           xxx(l)=(i-1)*dx+dx/2.0_wp-celx/2.0_wp
           yyy(l)=(j-1)*dy+dy/2.0_wp-cely/2.0_wp
           zzz(l)=(k-1)*dz+dz/2.0_wp-celz/2.0_wp

           ltype(l)=1

! set initial velocities & weight & lstfrz

           vxx(l)=0.0_wp
           vyy(l)=0.0_wp
           vzz(l)=0.0_wp

           weight(l)=1.0_wp
           lstfrz(l)=0

           l=l+1

        End Do
     End Do
  End Do

  natms=l-1

  dx=celx/Real((vatms_x-1),wp)
  dy=cely/Real((vatms_y-1),wp)
  dz=celz/Real((vatms_z-1),wp)

!    %---------------------------------------------------------------%
!    |  virtual particle on the Lower side and on the Upper side     |
!    %---------------------------------------------------------------%

  m=l+(vatms_x)*(vatms_y)

  Do i=1,vatms_x
     Do j=1,vatms_y

        xxx(l)=(i-1)*dx-celx/2.0_wp
        yyy(l)=(j-1)*dy-cely/2.0_wp
        zzz(l)=-celz/2.0_wp

        ltype(l)=1
        lstfrz(l)=1

        vxx(l)=0.0_wp
        vyy(l)=0.0_wp
        vzz(l)=0.0_wp

        weight(l)=1.0_wp


        xxx(m)=xxx(l)
        yyy(m)=yyy(l)
        zzz(m)=-zzz(l)

        ltype(m)=1
        lstfrz(m)=1

! Velocity of virtual particles on the upper side

        vxx(m)=0.517_wp
        vyy(m)=0.0_wp
        vzz(m)=0.0_wp

        weight(m)=1.0_wp

        l=l+1
        m=m+1

       safe=(m <= mxatms)
       If (.not.safe) Goto 1

     End Do
  End Do

!  l=m
!  m=l+(vatms_x)*(vatms_y)
!
!  Do i=1,vatms_x
!     Do j=1,vatms_y
!
!        xxx(l)=(i-1)*dx-celx/2.0_wp
!        yyy(l)=(j-1)*dy-cely/2.0_wp
!        zzz(l)=-celz/2.0_wp-dz/4.0_wp
!
!        ltype(l)=1
!        lstfrz(l)=1
!
!        vxx(l)=0.0_wp
!        vyy(l)=0.0_wp
!        vzz(l)=0.0_wp
!
!        weight(l)=1.0_wp
!
!        xxx(m)=xxx(l)
!        yyy(m)=yyy(l)
!        zzz(m)=-zzz(l)
!
!        ltype(m)=1
!        lstfrz(m)=1
!
!! Velocity of virtual particles on the upper side
!
!        vxx(m)=0.517_wp
!        vyy(m)=0.0_wp
!        vzz(m)=0.0_wp
!
!        weight(m)=1.0_wp
!
!        l=l+1
!        m=m+1
!
!        safe=(m <= mxatms)
!       If (.not.safe) Goto 1
!
!     End Do
!  End Do


!    %-------------------------------------------------------------%
!    |  virtual particle on the Left side and on the Right side    |
!    %-------------------------------------------------------------%

  l=m
  m=l+(vatms_y)*(vatms_z)-2*vatms_y

  Do j=1,vatms_y
     Do k=2,vatms_z-1

        xxx(l)=-celx/2.0_wp
        yyy(l)=(j-1)*dy-cely/2.0_wp
        zzz(l)=(k-1)*dz-celz/2.0_wp

        ltype(l)=1
        lstfrz(l)=1

        vxx(l)=0.0_wp
        vyy(l)=0.0_wp
        vzz(l)=0.0_wp

        weight(l)=1.0_wp

        xxx(m)=-xxx(l)
        yyy(m)=yyy(l)
        zzz(m)=zzz(l)

        ltype(m)=1
        lstfrz(m)=1

        vxx(m)=0.0_wp
        vyy(m)=0.0_wp
        vzz(m)=0.0_wp

        weight(m)=1.0_wp

        l=l+1
        m=m+1
        safe=(m <= mxatms)
       If (.not.safe) Goto 1

    End Do
  End Do

!  l=m
!  m=l+(vatms_y)*(vatms_z)
!
!  Do j=1,vatms_y
!     Do k=1,vatms_z
!
!        xxx(l)=-celx/2.0_wp-dx/4.0_wp
!        yyy(l)=(j-1)*dy-cely/2.0_wp
!        zzz(l)=(k-1)*dz-celz/2.0_wp
!
!        ltype(l)=1
!        lstfrz(l)=1
!
!        vxx(l)=0.0_wp
!        vyy(l)=0.0_wp
!        vzz(l)=0.0_wp
!
!        weight(l)=1.0_wp
!
!
!        xxx(m)=-xxx(l)
!        yyy(m)=yyy(l)
!        zzz(m)=zzz(l)
!
!        ltype(m)=1
!        lstfrz(m)=1
!
!        vxx(m)=0.0_wp
!        vyy(m)=0.0_wp
!        vzz(m)=0.0_wp
!
!        weight(m)=1.0_wp
!
!        l=l+1
!        m=m+1
!
!        safe=(m <= mxatms)
!       If (.not.safe) Goto 1
!
!     End Do
!  End Do


!    %-------------------------------------------------------------%
!    |  virtual particle on the front side and on the back side    |
!    %-------------------------------------------------------------%

  l=m

!  m=l+(vatms_x)*(vatms_z)
!
!  Do i=1,vatms_x
!     Do k=1,vatms_z
!
!        xxx(l)=(i-1)*dx-celx/2.0_wp
!        yyy(l)=-cely/2.0_wp
!        zzz(l)=(k-1)*dz-celz/2.0_wp
!
!        ltype(l)=1
!        lstfrz(l)=1
!
!        vxx(l)=0.0_wp
!       vyy(l)=0.0_wp
!       vzz(l)=0.0_wp
!
!        weight(l)=1.0_wp
!
!        l=l+1
!
!        xxx(m)=xxx(l)
!        yyy(m)=-yyy(l)
!        zzz(m)=zzz(l)
!
!        ltype(m)=1
!        lstfrz(m)=1
!
!        vxx(m)=0.0_wp
!       vyy(m)=0.0_wp
!       vzz(m)=0.0_wp
!
!        weight(m)=1.0_wp
!
!        m=m+1
!
!     End Do
!  End Do


1 nlast=m-1

  safe=(nlast <= mxatms)

  If (.Not. safe) Then
     Call warning(2,Real(nlast,wp),Real(mxatms,wp),0.0_wp)
     Call error(2)
  End If

End Subroutine cavity

!***********************************************************************

Subroutine hread (history)

  Use kinds_f90
  Use setup_module
  Use config_module, Only : natms,nlast,weight,ltype,lstfrz,                   &
                            xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz
  Use random_module

  Implicit None

  Character(Len = 40), Intent(In   ) :: history

  Real(Kind = wp) :: tstep, cell(1:9)

  Integer :: ktrj, nstep, matms, i, j

  Character(Len = 80) :: cfgname
  Character(Len =  8) :: atmnam, step

!     open history file if new job
  Rewind (Unit=nread)
  Read (Unit=nread, Fmt='(a80)', Err=200) cfgname
  Write (Unit=nrite, Fmt='(a, a)') '# INPUT file header: ',cfgname
  Read (Unit=nread, Fmt='(2i10)', End=200) ktrj, j

  Read (Unit=nread, Fmt='(3f20.12)', End=200) cell

  Do i = 1, natms
     Read (Unit=nread, Fmt='(a8, i10)', End=200) atmnam, j
     weight(i)=1.0_wp
     lstfrz(i)=0
     If (Trim(atmnam) == 'OW') Then
        ltype(i)=1
     Else If (Trim(atmnam) == 'OWW') Then
        ltype(i)=2
     End If
     Read (Unit=nread, Fmt='(3g20.10)', End=200) xxx(i),yyy(i),zzz(i)
     xxx(i)=xxx(i)/4.46341_wp
     yyy(i)=yyy(i)/4.46341_wp
     zzz(i)=zzz(i)/4.46341_wp
     If (ktrj .Ge. 1) Read (Unit=nread, Fmt='(3g20.10)', End=200) vxx(i),vyy(i),vzz(i)
     vxx(i)=kgauss()
     vyy(i)=kgauss()
     vzz(i)=kgauss()
     If (ktrj .Ge. 2) Read (Unit=nread, Fmt='(3g20.10)', End=200) fxx(i),fyy(i),fzz(i)
  End Do

  Close (Unit=nread)

  Return

100 Continue
  Write (Unit=nrite, Fmt='(a)') '# error - History file not found'
  Close (Unit=nread)
  Call error(0)

200 Continue
  Write (Unit=nrite, Fmt='(a)') '# warning - end of History file encountered'
  Close (Unit=nread)
  Return

End Subroutine

!***********************************************************************
