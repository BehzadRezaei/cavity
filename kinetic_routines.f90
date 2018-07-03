!    %---------------------THIS IS KINETIC_ROUTINES--------------------% 
!    |                                                                 |
!    | Function getkin - calculates the kinetic energy                 |
!    |                                                                 |
!    | Subroutine kinstress - calculates the kinetic stress            |
!    |                                                                 |
!    | Subroutine getcom - calculates the center of mass position      |
!    |                                                                 |
!    | Subroutine getvom - calculates the center of mass momentum      |
!    |                                                                 |
!    %-----------------------------------------------------------------% 

Function getkin(natms,weight,vxx,vyy,vzz)

!    %-----------------------------------------------------------------%
!    |  routine to calculate system kinetic energy                     |
!    %-----------------------------------------------------------------%

  Use kinds_f90
  Use config_module, Only : lstfrz

  Implicit None

  Integer,                             Intent( In    ) :: natms
  Real( Kind = wp ), Dimension( 1:* ), Intent( In    ) :: weight,vxx,vyy,vzz

  Integer                                              :: i
  Real( Kind = wp )                                    :: getkin,engke

  engke = 0.0_wp

  Do i=1,natms
     If (lstfrz(i) == 0) engke = engke + weight(i)*(vxx(i)**2+vyy(i)**2+vzz(i)**2)
  End Do

  getkin = 0.5_wp * engke

End Function getkin

!***********************************************************************

Subroutine kinstress(natms,weight,vxx,vyy,vzz,strkin)

!    %-----------------------------------------------------------------%
!    |  routine to calculate kinetic contribution to the stress tensor |
!    %-----------------------------------------------------------------%

  Use kinds_f90
  Use config_module, Only : lstfrz

  Implicit None

  Integer,                             Intent( In    ) :: natms
  Real( Kind = wp ), Dimension( 1:* ), Intent( In    ) :: weight,vxx,vyy,vzz
  Real( Kind = wp ), Dimension( 1:9 ), Intent(   Out ) :: strkin

  Integer :: i

  strkin = 0.0_wp

  Do i=1,natms
     If (lstfrz(i) == 0) Then
        strkin(1) = strkin(1) + weight(i)*vxx(i)*vxx(i)
        strkin(2) = strkin(2) + weight(i)*vxx(i)*vyy(i)
        strkin(3) = strkin(3) + weight(i)*vxx(i)*vzz(i)
        strkin(5) = strkin(5) + weight(i)*vyy(i)*vyy(i)
        strkin(6) = strkin(6) + weight(i)*vyy(i)*vzz(i)
        strkin(9) = strkin(9) + weight(i)*vzz(i)*vzz(i)
     End If
  End Do

! Symmetrise

  strkin(4) = strkin(2)
  strkin(7) = strkin(3)
  strkin(8) = strkin(6)

End Subroutine kinstress

!***********************************************************************

Subroutine getcom(natms,weight,xxx,yyy,zzz,com)

!    %-----------------------------------------------------------------%
!    |  routine to calculate system center of mass position            |
!    %-----------------------------------------------------------------%

  Use kinds_f90

  Implicit None

  Integer,                             Intent( In    ) :: natms
  Real( Kind = wp ), Dimension( 1:* ), Intent( In    ) :: weight,xxx,yyy,zzz
  Real( Kind = wp ), Dimension( 1:3 ), Intent(   Out ) :: com

  Logical,           Save :: newjob
  Data                       newjob /.true./
  Integer                 :: i
  Real( Kind = wp ), Save :: totmas

! total system mass

  If (newjob) Then
     newjob = .false.

     totmas = 0.0_wp

     Do i=1,natms
        totmas = totmas + weight(i)
     End Do

  End If

  com = 0.0_wp

  Do i=1,natms
     com(1) = com(1) + weight(i)*xxx(i)
     com(2) = com(2) + weight(i)*yyy(i)
     com(3) = com(3) + weight(i)*zzz(i)
  End Do

  com = com/totmas

End Subroutine getcom

!***********************************************************************

Subroutine getvom(natms,lstfrz,weight,vxx,vyy,vzz,vom)

!    %-----------------------------------------------------------------%
!    |  routine to calculate system center of mass momentum            |
!    %-----------------------------------------------------------------%

  Use kinds_f90

  Implicit None

  Integer,                             Intent( In    ) :: natms
  Integer,           Dimension( 1:* ), Intent( In    ) :: lstfrz
  Real( Kind = wp ), Dimension( 1:* ), Intent( In    ) :: weight,vxx,vyy,vzz
  Real( Kind = wp ), Dimension( 1:3 ), Intent(   Out ) :: vom

  Logical,           Save :: newjob
  Data                       newjob /.true./
  Integer                 :: i
  Real( Kind = wp ), Save :: totmas


  If (newjob) Then
     newjob = .false.

     totmas = 0.0_wp

     Do i=1,natms
        If (lstfrz(i) == 0) totmas = totmas + weight(i)
     End Do


  End If


  vom = 0.0_wp

  Do i=1,natms

     If (lstfrz(i) == 0) Then
        vom(1) = vom(1) + weight(i)*vxx(i)
        vom(2) = vom(2) + weight(i)*vyy(i)
        vom(3) = vom(3) + weight(i)*vzz(i)
     End If

  End Do

  vom = vom/totmas

End Subroutine getvom

!***********************************************************************
