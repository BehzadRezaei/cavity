Module config_module

!    %--------------------------------------------------------------------%
!    |  Module declaring global configuration variables and arrays        |
!    %--------------------------------------------------------------------%

  Use kinds_f90

  Implicit None

  Integer,               Save :: natms,nlast
  Data                           natms,nlast      / 2 * 0 /

  Real( Kind = wp ),     Save :: cell(1:9),volm,celx,cely,celz
  Data                           cell,     volm,celx,cely,celz / 13 * 0.0_wp /


  Integer,               Allocatable, Save :: lstfrz(:)
  Integer,               Allocatable, Save :: ltype(:)
  Integer,               Allocatable, Save :: ltg(:)
  Integer,               Allocatable, Save :: list(:,:)


  Real( Kind = wp ),     Allocatable, Save :: weight(:)
  Real( Kind = wp ),     Allocatable, Save :: xxx(:),yyy(:),zzz(:)
  Real( Kind = wp ),     Allocatable, Save :: vxx(:),vyy(:),vzz(:)
  Real( Kind = wp ),     Allocatable, Save :: fxx(:),fyy(:),fzz(:)
  Real( Kind = wp )                 , Save :: xxxmax,yyymax,zzzmax, &
                                              xxxmin,yyymin,zzzmin
  Real( Kind = wp ),     Allocatable, Save :: numtyp(:),dens(:)


  Public :: allocate_config_arrays

Contains

  Subroutine allocate_config_arrays()

    Use setup_module, Only : mxatms,mxlist,mxatyp

    Implicit None

    Integer, Dimension( 1:9 ) :: fail

    fail = 0

    Allocate (lstfrz(1:mxatms),                                   Stat = fail(1))
    Allocate (ltype(1:mxatms),                                    Stat = fail(2))
    Allocate (ltg(1:mxatms),                                      Stat = fail(3))
    Allocate (numtyp(1:mxatyp),dens(1:mxatyp),                    Stat = fail(4))
    Allocate (list(0:mxlist,1:mxatms),                            Stat = fail(5))
    Allocate (weight(1:mxatms),                                   Stat = fail(6))
    Allocate (xxx(1:mxatms),yyy(1:mxatms),zzz(1:mxatms),          Stat = fail(7))
    Allocate (vxx(1:mxatms),vyy(1:mxatms),vzz(1:mxatms),          Stat = fail(8))
    Allocate (fxx(0:mxatms),fyy(0:mxatms),fzz(0:mxatms),          Stat = fail(9))


    If (Any(fail > 0)) Call error(100)


    lstfrz = 0
    ltype  = 0
    ltg    = 0
    list   = 0

    numtyp = 0.0_wp      ;      dens   = 0.0_wp      ;      weight = 0.0_wp

    xxx    = 0.0_wp      ;      yyy    = 0.0_wp      ;      zzz    = 0.0_wp
    vxx    = 0.0_wp      ;      vyy    = 0.0_wp      ;      vzz    = 0.0_wp
    fxx    = 0.0_wp      ;      fyy    = 0.0_wp      ;      fzz    = 0.0_wp

    xxxmax = 0.0_wp      ;      yyymax = 0.0_wp      ;      zzzmax = 0.0_wp
    xxxmin = 0.0_wp      ;      yyymin = 0.0_wp      ;      zzzmin = 0.0_wp

  End Subroutine allocate_config_arrays

End Module config_module