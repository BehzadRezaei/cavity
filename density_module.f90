!    %-----------------------------------------------------------------%
!    |  D P D  --  version 2.0    /    January  2010     --    y.afshar
!    |             ya_afshar@me.iut.ac.ir
!    %-----------------------------------------------------------------%

!    %-----------------------------------------------------------------%
!    | module for accumulating & calculating statistics for density profile
!    %-----------------------------------------------------------------%

Module density_module

  Use kinds_f90
  Use setup_module

  Implicit None

  Real(Kind = wp), Dimension(:, :), Allocatable, Save :: densdz

  Real(Kind = wp), Save :: len
  Data                     len / 0.0_wp /

  Integer, Save :: mxgrdf0
  Data             mxgrdf0 / 0 /

  Integer, Save :: numdn
  Data             numdn / 0 /

  Private
  Public :: density_collect, density_compute, &
            allocate_density_arrays

Contains

  Subroutine allocate_density_arrays()

    Use config_module, Only : cell

    Implicit None

    Real(Kind = wp) :: len0

    Integer, Dimension(1:2) :: fail
    Integer                 :: mxgrdf1

    mxgrdf1=mxgrdf*2

    fail = 0

# 83 "density_module.b"
    ! length of cell in z directions
    len=Sum(Abs(cell(3:9:3)))
    len0=Max(len/Real(mxgrdf1, Kind=wp), 0.0_wp)
    mxgrdf0=Int(len/len0)

    Allocate (densdz(1:mxgrdf0, 1:mxatyp), Stat = fail(1))

    If (Any(fail > 0)) Call error(1017)

    densdz = 0.0_wp
# 106 "density_module.b"
  End Subroutine allocate_density_arrays

  !    %-----------------------------------------------------------------%
  !    | subroutine for accumulating statistic for x, y, z-density profile
  !    %-----------------------------------------------------------------%

  Subroutine density_collect()

    !    %-----------------------------------------------------------------%
    !    | author             - y.afshar         February    2010
    !    %-----------------------------------------------------------------%

    Use config_module,     Only : cell, natms, numtyp, &
                                  ltype, xxx, yyy, zzz

    Implicit None

    Real(Kind = wp) :: leno2
    Real(Kind = wp) :: rdelr

    Integer         :: i, k, l

    ! accumulator
    numdn=numdn+1

    ! half of x, y, z length
    leno2=0.5_wp*len

    ! grid interval for density profiles
    rdelr=Real(mxgrdf0, Kind=wp)/len

    ! set up atom iatm type and exclude it if absent crystallographically
    Do i=1, natms
       k=ltype(i)
       If (numtyp(k) > zero_plus) Then
# 163 "density_module.b"

          l=Int((zzz(i)+leno2)*rdelr + 1.0_wp)
          l=Max(1, l)
          l=Min(mxgrdf0, l)

          ! accumulate statistic
          densdz(l, k)=densdz(l, k) + 1.0_wp
# 180 "density_module.b"
       End If
    End Do

  End Subroutine density_collect

  !    %-----------------------------------------------------------------%
  !    | subroutine for calculating x, y, z-density profile from
  !    | accumulated data
  !    %-----------------------------------------------------------------%

  Subroutine density_compute(ns, time)

    !    %-----------------------------------------------------------------%
    !    | author             - y.afshar         February    2010
    !    %-----------------------------------------------------------------%

    Use config_module, Only : cell, volm, numtyp, ltype
    Use param_module,  Only : istden

    Implicit None

    Real(Kind = wp), Intent(In   ) :: time

    Integer,         Intent(in   ) :: ns

    Real(Kind = wp) :: delr, dvol, &
                       factor, rho, rho1, &
                       rrr, sum0, sum1

    Integer :: i, j, k, index

    Character(Len = 20) :: name
    Character(Len =  6) :: fname1
    Character(Len = 16) :: filname1

    Write(nrite, "(/, /, 12X, 'DENSITY PROFILES', /, /, &
                & 'calculated using ',i10,' configurations')") numdn
    Write(nrite, "(/, /)")

    ! open density files and write headers
    index=ns/istden
# 249 "density_module.b"
    fname1='ZDNDAT'
    Call getfn(fname1, index, 1, name)
    filname1=name(1:12)//'.plt'
    Open(Unit=nzdndt, File=filname1, Status='replace')

    Write(nzdndt, '(a)')'Variables = "z" , "density" '

    ! grid interval for density profiles
    delr=len/Real(mxgrdf0, Kind=wp)

    ! volumes of x, y & z strips
    dvol=(volm/len)*delr
# 276 "density_module.b"
    ! normalization factor
    numdn=Max(numdn, 1)




    factor =1.0_wp/(Real(numdn, Kind=wp)*dvol)

    ! for every species not absent crystallographically
    Do k=1, mxatyp

       If (numtyp(k) > zero_plus) Then

          Write(nrite, "(/,'rho(r)  :', i8, /, /)") k

          ! running integration of density
          sum0=0.0_wp

# 351 "density_module.b"
          Write(nzdndt, *) 'Zone T= Density  i=', mxgrdf0, 'F=Point',', SOLUTIONTIME=',time
          Write(nrite, "(/, 5x, 'r_z', 6x, 'rho_z', 9x, 'n(r)_z', /)")
          ! loop over distances
          Do j=1, mxgrdf0

             rrr=(Real(j, Kind=wp)-0.5_wp)*delr - 0.5_wp*len
             rho=densdz(j, k)*factor
             sum0=sum0 + rho*dvol

             ! null it if < 1.0e-6_wp
             If (rho < 1.0e-6_wp) Then
                rho1=0.0_wp
             Else
                rho1=rho
             End If
             If (sum0 < 1.0e-6_wp) Then
                sum1=0.0_wp
             Else
                sum1=sum0
             End If

             ! print out information
             Write(nzdndt, "(1p, 2e14.6)") rrr, rho
             Write(nrite, "(f10.4, 1p, 2e14.6, 0p)") rrr, rho1, sum1
          End Do
# 403 "density_module.b"
       End If

    End Do

    Write(nrite, "(/, /)")
# 436 "density_module.b"
    Write(nzdndt, *) 'Zone T= Density  i=', mxgrdf0, 'F=Point',', SOLUTIONTIME=',time
    Do j=1, mxgrdf0

       rrr=(Real(j, Kind=wp)-0.5_wp)*delr - 0.5_wp*len
       rho=Sum(densdz(j, 1:mxatyp)*factor)

       ! print out information
       Write(nzdndt, "(1p, 2e14.6)") rrr, rho

    End Do

    Close(Unit=nzdndt)
# 463 "density_module.b"
  End Subroutine density_compute

End Module density_module
