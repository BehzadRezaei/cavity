!    %----------------- THIS IS STATISTICS_ROUTINES -------------------%
!    |                                                                 |
!    | Subroutine  statistics_collect - subroutine for accumulating    |
!    |                                  data                           |
!    |                                                                 |
!    | Subroutine  statistics_result  - subroutine for writing         |
!    |                                  simulation summary             |
!    |                                                                 |
!    %-----------------------------------------------------------------%

Subroutine statistics_collect                                                  &
           (nsteql,keyres,intsta,nstep,tstep,time,tmst,pe,vir,str,ke)

!    %--------------------------------------------------------------%
!    |  subroutine for accumulating data during the DPD simulation  |
!    |  and computing the rolling averages                          |
!    %--------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module,  Only : cell,volm,natms,ltype,vxx,vyy,vzz,numtyp
  Use statistics_module
  Use param_module,   Only : imcon, nstvis, lden, nstden, istden
  Use viscosity_module
  Use density_module

  Implicit None

  Integer,           Intent( In    ) :: nsteql,keyres,intsta,nstep
  Real( Kind = wp ), Intent( In    ) :: tstep,time,pe,vir,str(1:9), ke
  Real( Kind = wp ), Intent( InOut ) :: tmst


  Logical,           Save :: newjob
  Data                       newjob / .true. /
  Logical                 :: l_tmp
  Integer                 :: fail,i,k,iadd,kstak
  Real( Kind = wp )       :: engcpe,vircpe,engke
  Real( Kind = wp )       :: sclnv1,sclnv2,zistk


  Real( Kind = wp ), Allocatable :: amsd(:)

  fail=0
  Allocate (amsd(1:mxatyp), Stat=fail)
  If (fail > 0) Then
     Write(nrite,'(/,1x,a)') 'statistics_collect allocation failure'
     Call error(0)
  End If

! open statistics file and put header

  If (newjob) Then
     newjob=.false.

! If the keyres=1 is the file old (does it exist)?

     l_tmp=.false.
     If (keyres == 1) Inquire(File='STATIS', Exist=l_tmp)

     If (.not.l_tmp) Then
        Open(nstats, File='STATIS', Status='replace')
        Write(nstats,'(5x,a5,6x,a4,12x,a4,7x,a6)')                             &
              'nstep','time','iadd','stpval'
        Close(nstats)
     End If
  End If

  iadd = 0

  engcpe = pe /Real(natms,wp)
  vircpe = vir/Real(natms,wp)
  engke  = ke /Real(natms,wp)

! store current values in statistics array

  stpval(1) = engcpe                                                    ! potential energy
  stpval(2) = vircpe                                                    ! virial
  stpval(3) = engke                                                     ! kinetic energy
  stpval(4) = stpval(1) + stpval(3)                                     ! total energy
  stpval(5) = Real(natms,wp)*(2.0_wp*stpval(3)-stpval(2))/(3.0_wp*volm) ! system pressure
  stpval(6) = 2.0_wp/3.0_wp*stpval(3)                                   ! system temperature

  iadd = iadd + 6

! pressure tensor (derived for the stress tensor)

  Do i=1,9
     stpval(iadd+i)=str(i)/volm
  End Do

  iadd = iadd + 9

! mean squared displacements per species, dependent on
! particle displacements from initial positions (at t=0)

  amsd = 0.0_wp ! initialize

  If (nstep == nsteql+1) Then ! re-initialize
     Do i=1,natms
        xto(i) = 0.0_wp
        yto(i) = 0.0_wp
        zto(i) = 0.0_wp
     End Do
  End If

  If (nstep > 0) Then
     Do i=1,natms
        xto(i)=xto(i)+vxx(i)*tstep
        yto(i)=yto(i)+vyy(i)*tstep
        zto(i)=zto(i)+vzz(i)*tstep

        rsd(i)=Sqrt(xto(i)**2+yto(i)**2+zto(i)**2)

        k=ltype(i)
        amsd(k)=amsd(k)+rsd(i)**2
     End Do
  End If

  Do k = 1,mxatyp
     stpval(iadd+k)=amsd(k)/Max(numtyp(k),1.0_wp)
  End Do

  iadd = iadd + mxatyp

! write statistics file

  If (Mod(nstep,intsta) == 0) Then
     Open(nstats, File='STATIS', Position='append')

     Write(nstats,'(i10,1x,1p,e14.6,0p,i10,/,(1p,7e14.6))')                    &
          nstep,time,iadd,(stpval(k),k=1,iadd)

     Close(nstats)
  End If

! check on number of variables for stack

  If (iadd > mxnstk) Call error(170)

! No totals for timestep zero

  If (nstep == 0) Go To 10


!  %----------------------------------------------------------------------%
!  |   stpval(mxnstk)-instantaneous values of thermodynamic variables     |
!  |                                                                      |
!  |   zumval(mxnstk)-running totals of thermodynamic variables           |
!  |                                                                      |
!  |   stkval(mxstak,mxnstk)-stacked values of thermodynamic variables    |
!  |                                                                      |
!  |   ravval(mxnstk)-rolling averages of thermodynamic variables         |
!  |                                                                      |
!  |   sumval(mxnstk)-average values of thermodynamic variables           |
!  |                                                                      |
!  |   ssqval(mxnstk)-fluctuation (squared) of thermodynamic variables    |
!  |                                                                      |
!  %----------------------------------------------------------------------%


! current stack value

  kstak=Mod(nstep-1,mxstak)+1

! subtract old stack value from the stack average

  If (nstep > mxstak) Then
     Do i=1,mxnstk
        zumval(i)=zumval(i)-stkval(kstak,i)
     End Do
  End If

! store quantities in stack and update the stack average

  Do i=1,mxnstk
     stkval(kstak,i)=stpval(i)
     zumval(i)=zumval(i)+stpval(i)
  End Do

! calculate rolling averages

  zistk=Min(mxstak,nstep)

  Do i=1,mxnstk
     ravval(i)=zumval(i)/zistk
  End Do

! accumulate totals over steps

  If (nstep > nsteql) Then
     numacc=numacc+1

     sclnv1=Real(numacc-1,wp)/Real(numacc,wp)
     sclnv2=1.0_wp/Real(numacc,wp)

! average squared sum and sum (keep in this order!!!)

     If (nstep == nsteql+1) stpvl0=stpval
     stpval=stpval-stpvl0
     Do i=1,mxnstk
        ssqval(i)=sclnv1*(ssqval(i)+sclnv2*(stpval(i)-sumval(i))**2)

! sumval has to be shifted back to sumval+stpvl0 in statistics_result
! when averaging is printed since stpval is only shifted back and forth
! which does not affect the fluctuations Sqrt(ssqval) only their accuracy

        sumval(i)=sclnv1*sumval(i)+sclnv2*stpval(i)
     End Do
     stpval=stpval+stpvl0
  End If

10 Continue

  If (imcon == 4 .and. nstep > nsteql + nstvis) Call viscosity_collect(nstep, str)


  If (lden .and. nstep > nsteql) Then
     If (Mod(nstep, nstden) == 0) Call density_collect()

! calculate and print density distribution
     If (Mod(nstep-nsteql, istden) == 0) Call density_compute(nstep-nsteql, time)
  End If


! Catch time of starting statistical averages

  If ((nstep == nsteql) .and. tmst < tstep) tmst=time

  Deallocate (amsd, Stat=fail)
  If (fail > 0) Then
     Write(nrite,'(/,1x,a)') 'statistics_collect deallocation failure'
     Call error(0)
  End If

End Subroutine statistics_collect

!***********************************************************************

Subroutine statistics_result                                                   &
           (rcut,lrdf,nstep,tstep,time,tmst)

!    %----------------------------------------------%
!    |  subroutine for writing simulation summary   |
!    %----------------------------------------------%


  Use kinds_f90
  Use setup_module
  Use config_module,     Only : cell,volm,dens
  Use statistics_module
  Use viscosity_module

  Implicit None

  Logical,           Intent( In    ) :: lrdf
  Integer,           Intent( In    ) :: nstep
  Real( Kind = wp ), Intent( In    ) :: rcut,tstep,time,tmst

  Logical           :: check
  Integer           :: i,j,iadd
  Real( Kind = wp ) :: avvol,avcel(1:9),dc,srmsd,timelp,tmp

! dry run

  If (nstep == 0) Go To 10

! shift back statistical averages as from statistics_collect

  Do i=1,mxnstk
     sumval(i)=sumval(i)+stpvl0(i)
  End Do

! average volume

  avvol = volm

! average cell

  avcel = cell

! calculate final fluctuations

  Do i=1,mxnstk
     ssqval(i)=Sqrt(ssqval(i))
  End Do

! Get elapsed time

  Call gtime(timelp)

! Get simulation time for averages

  If (numacc == 0) Then
     tmp=0.0_wp
  Else
     tmp=time-tmst
  End If

! final averages and fluctuations

  Write(nrite,"(/,/,1x,'run terminated after',i9,' steps (',f10.3,             &
       ' dpd_sec), final averages calculated over',i9,' steps (',f10.3,        &
       ' dpd_sec).',/,/)") nstep,time,numacc,tmp

  If (numacc == 0) Go To 10 ! No averages when running pure equilibration

  Write(nrite,"(1x,130('-'),/)")

  Write(nrite,'(12x,"pe-total",5x,"vir-total",4x,"ke-total",5x                 &
                "en-total",5x,"pressure",4x,"temperature",/)')

  Write(nrite,'(8x,1p,6(1x,e12.4))') (sumval(i),i=1,6)

  Write(nrite,"(/,1x,' r.m.s.',1p,6(1x,e12.4),/,1x,'fluctn. ')")               &
       (ssqval(i),i=1,6)

  Write(nrite,"(1x,130('-'))")

  If (numacc > 0) Then

     iadd = 15

! Write out estimated diffusion coefficients

     Write(nrite,"(/,/,1x,a)")                                                 &
                 'Approximate 3D Diffusion Coefficients and square root of MSDs'
     Write(nrite,"(/,8x,'Particle',6x,'DC (L^2 t^-1)',4x,                      &
                 'Sqrt[MSD] (dpd_units)',/)")

     Do i = 1, mxatyp
        dc = (ravval(iadd+i)-sumval(iadd+i)) /                                 &
             (3.0_wp*Real(numacc-Min(mxnstk,numacc-1),wp)*tstep)
        If (dc < 1.0e-10_wp) dc = 0.0_wp

        srmsd = Sqrt(ravval(iadd+i))
        Write(nrite,'(8x,i6,1p,2(7x,e13.4))')i,dc,srmsd
     End Do

  Write(nrite,"(1x,130('-'))")

  iadd = iadd+mxatyp

  End If

! scale densities for average volume and averge volume and cell

  Do i = 1,mxatyp
     dens(i)=dens(i)*(volm/avvol)
  End Do

  volm = avvol
  cell = avcel

! calculate and print radial distribution functions

  If (lrdf .and. numrdf > 0) Call rdf_compute(rcut)

  If (numvs > 0) Call viscosity_compute(nstep)

10 Continue

! print final time check

  Call gtime(timelp)

  Write(nrite,'(/,/,/,1x,"time elapsed since job start: ", f12.3, " sec",/)') timelp

End Subroutine statistics_result