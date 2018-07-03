
Subroutine set_bounds

!    %--------------------------------------------------------%
!    | subroutine to determine various limits: array bounds   |
!    %--------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : cell, natms, nlast, volm,  &
                            celx, cely, celz
  Use param_module,  Only : x1_geom, y1_geom, z1_geom, &
                            x2_geom, y2_geom, z2_geom, &
                            x3_geom, y3_geom, z3_geom, &
                            natms_x, natms_y, natms_z, &
                            rcut, config_input

  Implicit None

  Real( Kind = wp ) :: celprp(1:10), dens0, volm0, cut, new, xyz

  Integer :: ilx, ily, ilz, ncells, i, uvw,  ktrj, imcon

  Character(Len = 8) :: step

  Logical :: safe1, safe2, safe3

!                Determine various limits:
! array bounds,iteration and others as specified in setup_module

  cell(1)=x1_geom
  cell(2)=y1_geom
  cell(3)=z1_geom
  cell(4)=x2_geom
  cell(5)=y2_geom
  cell(6)=z2_geom
  cell(7)=x3_geom
  cell(8)=y3_geom
  cell(9)=z3_geom

  Call dcell(cell,celprp)

  celx=celprp(7)
  cely=celprp(8)
  celz=celprp(9)
  volm=celprp(10)

! PREALLOCATION OF NUMBER OF ATOMS

  If (config_input) Then

     Inquire (File='HISTORY0',  Exist=safe1)
     Inquire (File='RESCONFIG', Exist=safe2)
     Inquire (File='CONFIG',    Exist=safe3)

     If (safe1) Then

        ! READING FROM TRAJECTORY FILE
        Open (Unit=nread, File='HISTORY0', Form='formatted')
        Read (Unit=nread, Fmt='(i10,2f12.4)') uvw,xyz,xyz
        Read (Unit=nread, Fmt='(2i10,i3)') natms,nlast,uvw
        Read (Unit=nread, Fmt='(3f20.10)') cell(1),cell(2),cell(3)
        Read (Unit=nread, Fmt='(3f20.10)') cell(4),cell(5),cell(6)
        Read (Unit=nread, Fmt='(3f20.10)') cell(7),cell(8),cell(9)

     Else If (safe2) Then

        ! READING FROM CONFIG_WRITE OUTPUT ROUTINES(RESCONFIG)
        Open (Unit=nread, File='RESCONFIG', Form='formatted')
        Read (Unit=nread, Fmt='(i3,i10,2f15.8,2i10)') xyz,uvw,xyz,xyz,natms,nlast
        Read (Unit=nread, Fmt='(3f20.10)') cell(1),cell(2),cell(3)
        Read (Unit=nread, Fmt='(3f20.10)') cell(4),cell(5),cell(6)
        Read (Unit=nread, Fmt='(3f20.10)') cell(7),cell(8),cell(9)

     Else If (safe3) Then

        Open (Unit=nread, File='CONFIG', Status='Old')
        Read (Unit=nread, Fmt=*)
        Read (Unit=nread, Fmt='(2i10)') ktrj, imcon
        Read (Unit=nread, Fmt='(3f20.12)') celprp(1:9)
        i=0
        Do
           Read (Unit=nread, Fmt=*, End=5)
           Read (Unit=nread, Fmt=*, End=5)
           If (ktrj .Ge. 1) Read (Unit=nread, Fmt=*, End=5)
           If (ktrj .Ge. 2) Read (Unit=nread, Fmt=*, End=5)
           i=i+1
        End Do

5       natms=i
        nlast=natms

     End If

     Call dcell(cell, celprp)
     volm0=celprp(10)
     dens0=Real(natms/volm0, wp)
     new=(dens0)**(-1.0_wp/3.0_wp)
     Goto 10

  Else

! number of atoms in each direction of geometry cell without considering the type
!    This is the maximum possible number of atoms
!    n1*n2*n3 <------> dens= (n_atms / vol)
     natms=natms_x*natms_y*natms_z
     dens0=Real(natms/volm,wp)

  End If

10 Continue

!    %--------------------------------------------------------%
!    |                                                        |
!    |    !******!!  YOU SHOULD NOT EDIT BELOW  !!******!     |
!    |                                                        |
!    %--------------------------------------------------------%

! define cut

  cut=rcut+1.0e-6_wp

! calculate link cell dimensions

  ilx=Int(celx/cut)
  ily=Int(cely/cut)
  ilz=Int(celz/cut)
  ncells=ilx*ily*ilz

! print link cell algorithm and check for violations

  If (ncells == 0) Call error(20)
  If (ilx < 4 .or. ily < 4 .or. ilz < 4) Call warning(25,0.0_wp,0.0_wp,0.0_wp)

! set maximum number of atoms

  mxatms = Int( volm/Real(ncells,wp)*dens0*Real((ilx+4)*(ily+4)*(ilz+4),wp) )

! set maximum number of list

  mxlist = Nint(6.0_wp * dens0 * 1.7_wp *pi*cut**3)

! set maximum number of cells

  mxcell = (ilx+3)*(ily+3)*(ilz+3)

End Subroutine set_bounds
