Module statistics_module

!    %--------------------------------------------------------------------%
!    |  module declaring global simulation property variables and arrays  |
!    %--------------------------------------------------------------------%

  Use kinds_f90

  Implicit None

  Integer,                        Save :: numacc,numrdf,ntprdf
  Data                                    numacc,numrdf,ntprdf / 3 * 0 /


  Integer,           Allocatable, Save :: lstrdf(:)

  Real( Kind = wp ), Allocatable, Save :: xin(:),yin(:),zin(:)
  Real( Kind = wp ), Allocatable, Save :: xto(:),yto(:),zto(:)

  Real( Kind = wp ), Allocatable, Save :: rdf(:,:),rsd(:)
  Real( Kind = wp ), Allocatable, Save :: stpval(:),stpvl0(:),sumval(:),ssqval(:)
  Real( Kind = wp ), Allocatable, Save :: zumval(:),ravval(:),stkval(:,:)

  Public :: allocate_statistics_arrays

Contains

  Subroutine allocate_statistics_arrays()

    Use setup_module, Only : mxatms,mxatyp,mxrdf,mxgrdf,mxnstk,mxstak

    Implicit None

    Integer, Dimension( 1:6 ) :: fail

    fail = 0

    Allocate (lstrdf(1:mxrdf),                                                     Stat = fail(1))
    Allocate (xin(1:mxatms),yin(1:mxatms),zin(1:mxatms),                           Stat = fail(2))
    Allocate (xto(1:mxatms),yto(1:mxatms),zto(1:mxatms),rsd(1:mxatms),             Stat = fail(3))
    Allocate (rdf(1:mxgrdf,1:mxrdf),                                               Stat = fail(4))
    Allocate (stpval(1:mxnstk),stpvl0(1:mxnstk),sumval(1:mxnstk),ssqval(1:mxnstk), Stat = fail(5))
    Allocate (zumval(1:mxnstk),ravval(1:mxnstk),stkval(1:mxstak,1:mxnstk),         Stat = fail(6))

    If (Any(fail > 0)) Call error(1016)

    lstrdf = 0

    xin = 0.0_wp ; yin = 0.0_wp ; zin = 0.0_wp
    xto = 0.0_wp ; yto = 0.0_wp ; zto = 0.0_wp ; rsd = 0.0_wp

    rdf = 0.0_wp 

    stpval = 0.0_wp ; stpvl0 = 0.0_wp ; sumval = 0.0_wp ; ssqval = 0.0_wp
    zumval = 0.0_wp ; ravval = 0.0_wp ; stkval = 0.0_wp

  End Subroutine allocate_statistics_arrays

End Module statistics_module