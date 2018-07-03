!    %-----------------------------------------------------------------%
!    |  D P D  --  version 2.0    /    January  2010     --    y.afshar
!    |             ya_afshar@me.iut.ac.ir
!    %-----------------------------------------------------------------%

!    %-----------------------------------------------------------------%
!    | module for defining global external field variables and arrays
!    %-----------------------------------------------------------------%

Module external_field_module

  Use kinds_f90, Only : wp

  Implicit None

  Real(Kind = wp), Dimension(:), Allocatable, Save :: prmfld

  ! Only one type of field can be applied on the system (keyfld is a scalar)
  Integer,                                    Save :: keyfld = 0

  Public :: allocate_external_field_arrays, &
            external_field_apply, &
            external_field_correct

Contains

  Subroutine allocate_external_field_arrays()

    Use setup_module, Only : mxpfld

    Implicit None

    Integer, Dimension(1:1) :: fail

    fail = 0

    Allocate (prmfld(mxpfld), Stat = fail(1))

    If (Any(fail > 0)) Call error(1019)

    prmfld = 0.0_wp

  End Subroutine allocate_external_field_arrays

  !    %-----------------------------------------------------------------%
  !    | subroutine for application of an external field
  !    |
  !    | Note: Only one field at a time is allowed
  !    %-----------------------------------------------------------------%

  Subroutine external_field_apply(imcon, tstep, engfld, virfld)

    !    %-----------------------------------------------------------------%
    !    | dl_poly_3          copyright - daresbury laboratory
    !    | author             - i.t.todorov      july        2007
    !    | amended            - y.afshar         January     2010
    !    %-----------------------------------------------------------------%

    Use setup_module,  Only : pi
    Use config_module

    Implicit None

    Real(Kind = wp), Intent(In   ) :: tstep
    Real(Kind = wp), Intent(  Out) :: engfld, virfld

    Integer,         Intent(In   ) :: imcon

    Real(Kind = wp), Dimension(1:3) :: eprmfld
    Real(Kind = wp) :: rz

    Integer         :: i

    ! energy and virial accumulators

    engfld=0.0_wp
    virfld=0.0_wp

    eprmfld=0.0_wp

    Select Case (keyfld)

       Case (1)

          ! electric field: prmfld(1-3) are field components

       Case (2)

          ! oscillating shear: orthorhombic box:  Fx=a*Cos(b.2.pi.z/L)
          If (imcon /= 1) Go To 10

          rz=2.0_wp*pi/cell(9)

          Do i=1, natms
             If (lstfrz(i) == 0) &
                 fxx(i)=fxx(i) + prmfld(1)*Cos(prmfld(2)*zzz(i)*rz)
          End Do

       Case (3)

          ! continuous shear of walls : 2D periodic box (imcon=2)
          If (imcon /= 2 .or. imcon /= 4) Go To 10

          ! shear rate=prmfld(1) DPD_length per DPD_time for non-frozen
          ! and non-weightless atoms at Abs(z) > prmfld(2)

          Do i=1, natms
             If (lstfrz(i) == 0 .and. weight(i) > 1.0e-6_wp .and. Abs(zzz(i)) > prmfld(2)) &
                vxx(i)=0.5_wp*Sign(prmfld(1), zzz(i))
          End Do

       Case (4)

          ! gravitational field: field components given by prmfld(1-3)
          eprmfld(1:3)=prmfld(1:3)
          Do i=1, natms
             If (lstfrz(i) == 0) Then
                fxx(i)=fxx(i)+eprmfld(1)*weight(i)
                fyy(i)=fyy(i)+eprmfld(2)*weight(i)
                fzz(i)=fzz(i)+eprmfld(3)*weight(i)
             End If
          End Do

       Case (5)
          ! Poiseuille flow method using priodicity
          ! for measuring viscosity of the Dpd or MD fluid
          eprmfld(1)=prmfld(1)
          Do i=1, natms

             If (lstfrz(i) == 0) Then

                fzz(i)=fzz(i)+Sign(eprmfld(1), xxx(i))*weight(i)

             End If

          End Do

       Case Default

          ! unidentified field potential error exit
          Call error(454)

    End Select

10  Continue

  End Subroutine external_field_apply

  !    %-----------------------------------------------------------------%
  !    | subroutine for correcting an external field application
  !    |
  !    | Note: Only one field at a time is allowed
  !    %-----------------------------------------------------------------%

  Subroutine external_field_correct(imcon)

    !    %-----------------------------------------------------------------%
    !    | dl_poly_3          copyright - daresbury laboratory
    !    | author             - i.t.todorov      july        2007
    !    | amended            - y.afshar         January     2010
    !    %-----------------------------------------------------------------%

    Use config_module

    Implicit None

    Integer, Intent(In   ) :: imcon

    Integer :: i

    If (keyfld == 3) Then

       ! continuous shear of walls : 2D periodic box (imcon=2)
       If (imcon /= 2) Go To 10

       ! shear rate=prmfld(1) DPD_length per DPD_time for non-frozen
       ! and non-weightless atoms at Abs(z) > prmfld(2)

       Do i=1, natms
          If (lstfrz(i) == 0 .and. weight(i) > 1.0e-6_wp .and. Abs(zzz(i)) > prmfld(2)) &
          vxx(i)=0.5_wp*Sign(prmfld(1), zzz(i))
       End Do

    End If

10  Continue

  End Subroutine external_field_correct


End Module external_field_module
