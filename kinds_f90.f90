Module kinds_f90

!    %--------------------------------------------------------------------%
!    |  module setting global working precision for Real numbers and      |
!    |  double precision for Integer numbers at compile time              |
!    %--------------------------------------------------------------------%

  Implicit None

  Integer, Parameter :: wp = Selected_Real_Kind(14,300)
  Integer, Parameter :: ip = Selected_Int_Kind(12)

End Module kinds_f90
