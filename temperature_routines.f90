!    %-------------------THIS IS TEMPERATURE_ROUTINES------------------% 
!    |                                                                 |
!    | Subroutine set_temperature   -                                  |
!    |                                                                 |
!    | Subroutine scale_temperature -                                  |
!    |                                                                 |
!    %-----------------------------------------------------------------% 

Subroutine set_temperature (imcon,kt,keyres)

  !    %-----------------------------------------------------------------%
  !    |  subroutine for setting the initial system temperature          |
  !    %-----------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : natms,lstfrz,weight,vxx,vyy,vzz

  Implicit None

  Integer,           Intent( In    ) :: imcon,keyres
  Real( Kind = wp ), Intent( In    ) :: kt

  Real( Kind = wp )                  :: totmas
  Integer                            :: i,k
  Real( Kind = wp )                  :: tmp,vom(1:3),engke,getkin

  If (keyres == 1) Return

! total system mass

  totmas = 0.0_wp
  k = 0

  Do i=1,natms
     If (lstfrz(i) == 0) Then
        totmas = totmas + weight(i)
        k = k + 1
     End If
  End Do

! calculate system center of mass momentum
  Call getvom(natms,lstfrz,weight,vxx,vyy,vzz,vom)

  engke = 2.0_wp*getkin(natms,weight,vxx,vyy,vzz)

  tmp = 0.5_wp*2.0_wp/3.0_wp*(engke-totmas*(vom(1)**2+vom(2)**2+vom(3)**2))/Real(k,wp)

  If (tmp < 1.0e-6_wp) Then
     tmp=0.0_wp
  Else
     tmp = Sqrt(kt/tmp)
  End If

! remove centre of mass motion and scale to temperature  

  Do i=1,natms
     If (lstfrz(i) == 0) Then
        vxx(i) = tmp * (vxx(i) - vom(1))
        vyy(i) = tmp * (vyy(i) - vom(2))
        vzz(i) = tmp * (vzz(i) - vom(3))
     Else
        vxx(i) = 0.0_wp
        vyy(i) = 0.0_wp
        vzz(i) = 0.0_wp
     End If
  End Do

End Subroutine set_temperature

!***********************************************************************

Subroutine scale_temperature(imcon,nstep)

  !    %-----------------------------------------------------------------%
  !    |  Subroutine to scale the instantenious system temperature       |
  !    |  to the target temperature                                      |
  !    %-----------------------------------------------------------------%

  Use kinds_f90
  Use config_module,       Only : natms,vxx,vyy,vzz
  Use param_module,        Only : kt,ltemp,nstscal,nsteql
  Use statistics_module,   Only : stpval

  Implicit None

  Integer,           Intent( In    ) :: imcon,nstep

  Real( Kind = wp ) :: tmp
  Integer           :: i

  If (nstep > nsteql) ltemp = .false.
  If (nstep <= nsteql .and. ltemp) Then

     If (Mod(nstep,nstscal) == 0) Then

! apply temperature scaling

        tmp=Sqrt(kt/stpval(6))

        Do i=1,natms
           vxx(i)=vxx(i)*tmp
           vyy(i)=vyy(i)*tmp
           vzz(i)=vzz(i)*tmp
        End Do

     End If

  End If

End Subroutine scale_temperature
