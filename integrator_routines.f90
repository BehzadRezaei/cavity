!    %-----------------------------------------------------------------%
!    |  D P D  --  version 1.4    /    December  2008  --   y.afshar   |
!    %-----------------------------------------------------------------%

!    %----------------- THIS IS INTEGRATOR_ROUTINE --------------------%
!    |                                                                 |
!    | Subroutine  time_integration                                    |
!    |                                                                 |
!    | Subroutine  vv - velocity verlet algorithm                      |
!    |                                                                 |
!    | Subroutine  vv_m - modified velocity verlet algorithm           |
!    |                                                                 |
!    %-----------------------------------------------------------------%

Subroutine time_integration(maxtimestep,time,tmst,dnstep)

!    %--------------------------------------------------------------%
!    |  routine to integrate the equation of motion                 |
!    %--------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module
  Use param_module
  Use statistics_module
  Use external_field_module

  Implicit None

  Integer,                Intent( In    ) :: maxtimestep
  Integer,                Intent( InOut ) :: dnstep
  Real( Kind = wp ),      Intent( InOut ) :: tmst,time

  Logical,                           Save :: newjob,newjob1
  Data                                       newjob,newjob1  / .True. , .True. /
  Integer,                           Save :: nstart,nlast0
  Data                                       nstart,nlast0 / 2*0 /
  Real( Kind = wp )                       :: strkin(1:9),engke,                &
                                             stress(1:9)
  Integer                                 :: current_ts,i,j,k,isw,nstep
  Real( kind = wp )                       :: timelp,engcpe,vircpe,             &
                                                    engfld,virfld

  nstep=dnstep

  If (newjob) Then
     newjob=.false.

     nlast0 = nlast

     If (keyres == 1) nstart=nstep
     If (ltraj .and. keyres /= 1)                                              &
        Call trajectory_write(imcon,keyres,ndump,nstraj,istraj,nstep,tstep,time)

! Write out system data

     Write(nrite,'(/,/,2x,a,/,/)')'Physical Specification'
     Write(nrite,'(10x,a,1p,e16.8)')'system volume               =',volm
     Write(nrite,'(10x,a,1p,e12.4)')'system temperature          =',kt
     Write(nrite,'(10x,a,1p,e12.4)')'simulation timestep         =',tstep
     Do i = 1, mxatyp
        k=i*(i-1)/2+i
        Write(nrite,'(/,10x,a,i3,a,i8,a,f12.3)')                               &
             'particle type',i,' population =', Int(numtyp(i)),                &
             '   &   Repulsion Parameter =', vvv(1, k)
     End Do
     Do i = 1, mxatyp
        Do j = i, mxatyp
           If (j /= i) Then
              k=j*(j-1)/2+i
              Write(nrite,'(/,10x,a,i3,a,i3,a,f12.3)')                         &
                   'Repulsion Parameter between Paricle type', i,              &
                   ' &', j, '  =', vvv(1, k)
           End If
        End Do
     End Do
     Write(nrite,'(/,/,2x,a,/,/)')'Simulation Controls'
     Write(nrite,'(10x,a,i10,a,i10)')                                          &
          'system size (particles)        =',natms,' ,',nlast
     Write(nrite,'(10x,a,i2)')'restart control parametr       =',keyres
     Write(nrite,'(10x,a,i4)')'printing interval              =',print_step
     Write(nrite,'(10x,a,i4)')'saving interval                =',save_step
     Write(nrite,'(10x,a,i4)')'stacking interval              =',mxstak

     If (ltemp) Then
        Write(nrite,'(10x,a)')'temperature scaling option     = true'
     Else
        Write(nrite,'(10x,a)')'temperature scaling option     = false'
     End If

     Write(nrite,'(10x,a,i4)')'temperature scaling interval   =',nstscal
     Write(nrite,'(10x,a,i10)')'equilibration period           =',nsteql
     Write(nrite,'(10x,a,1p,e12.4)')'cutoff radious                 =',rcut
     Write(nrite,'(10x,a,1p,e12.4)')'cutoff radious for vdw fluid   =',rvdw
     If (ex_force) Then
        Write(nrite,'(/,/)')
        Write(nrite,'(10x,a,1p,5e12.4)')'external force parameters      =',prmfld
        Write(nrite,'(/,/)')
     Else
        Write(nrite,'(/,/)')
     End If

     Do i = 1, mxatyp
        k=i*(i-1)/2+i
        Write(nrite,'(/,10x,a,i3,a,f12.3)')                                    &
             'particle type',i,' coefficient of viscosity =', vvv(2, k)

        Write(nrite,'(10x,a,i3,a,f12.3)')                                      &
             'particle type',i,' random force parameter   =',                  &
             (2.0_wp*vvv(2, k)*kt)**0.5_wp
     End Do

     Do i = 1, mxatyp
        Do j = i, mxatyp
           If (j /= i) Then
              k=j*(j-1)/2+i
              Write(nrite,'(/,10x,a,i3,a,i3,a,f12.3)')                         &
                   'coefficient of viscosity between Paricle type', i,         &
                   ' &', j, '  =', vvv(2, k)
              Write(nrite,'(10x,a,i3,a,i3,a,f12.3)')                           &
                   'random force parameter between Paricle type  ', i,         &
                   ' &', j, '  =', (2.0_wp*vvv(2, k)*kt)**0.5_wp
           End If
        End Do
     End Do

     Write(nrite,'(/,/,2x,a,/)')'Boundary Condition'

     Select Case (imcon)
        Case(0)
           Write(nrite,'(10x,a,/,/)') 'imcon=0 no specified boundary conditions applied'
        Case(1)
           Write(nrite,'(10x,a,/,/)') 'imcon=1 orthorhombic boundaries applied'
        Case(2)
           Write(nrite,'(10x,a,/,/)') 'imcon=2 Slab (x,y periodic, z non-periodic)'
        Case(3)
           Write(nrite,'(10x,a,/,/)') '2D (y periodic, x,z non-periodic)'
        Case(4)
           Write(nrite,'(10x,a,/,/)') 'Lees Edwards (x,y periodic, z uniform shear)'
        Case Default
           Write(nrite,'(10x,a,/,/)') 'New Boundary conditin is defined, !!! Becareful !!!'
     End Select

  End If

! If maxtimestep == 0, program uses the default value

  If (maxtimestep /= 0)  Then
     nstrun=maxtimestep
  Else If (maxtimestep == 0) Then
          nstrun=nstrund
  End If

  current_ts=0

!  %----------------------------%
!  |  start of DPD calculations |
!  %----------------------------%

  Do nstep=nstart+1,nstart+nstrun

     current_ts=current_ts+1

     If (lcout) Call conout(nstep-1,time)

! Update total time of simulation
     time=time+tstep

! Integrate equations of motion - first stage of integration algorithm

     isw = 0
     If (l_vv == 0) Then

        Call vv(isw,tstep,strkin,engke)

     Else If (l_vv == 1) Then

        Call vv_m(isw,tstep,strkin,engke)

     End If

! Update positions of particles

     Call pbcshift(imcon,time)

! Implementing Boundary condition

!     Call bcshift(imcon,mdir,tstep,time)

     Call bounce_back(imcon,tstep)

! Exchange atomic data in border regions
! (for oneway periodic or double periodic boundary condition, imcon=2 or 3 )

     Call set_halo_particles(imcon,rcut,time)

! Initialize force arrays (these are aditive in the force subroutine)

     fxx = 0.0_wp
     fyy = 0.0_wp
     fzz = 0.0_wp

! Initialize stress tensor

     stress = 0.0_wp

! Computing Forces

     Call force(nstep,engcpe,vircpe,stress,nlast0)

     nlast = nlast0

     If (ex_force) Call external_field_apply(imcon, tstep, engfld, virfld)

! Integrate equations of motion - second stage integration algorithm

     isw = 1

     If (l_vv == 0) Then

        Call vv(isw,tstep,strkin,engke)

     Else If (l_vv == 1) Then

        Call vv_m(isw,tstep,strkin,engke)

     End If

! Implementing Boundary condition

!     Call bcshift(imcon,mdir,tstep,time)

     Call bounce_back(imcon,tstep)

! Calculate physical quantities and collect statistics

     Call statistics_collect                                                   &
          (nsteql,keyres,intsta,nstep,tstep,time,tmst,engcpe,vircpe,stress,engke)


     Call scale_temperature(imcon,nstep)

! Printer output at print_step interval

     If (Mod(nstep,print_step).eq.0) Then

! Update cpu time

        Call gtime(timelp)

        If (newjob1) Then
           newjob1 = .false.
           Write(nrite,"(1x,130('-'),/,/,                                      &
                10x,'step',5x,'time(dpd_sec)',                                 &
                7x,'en-total',7x,'pe-total',6x,'vir-total',                    &
                6x,'ke-total',7x,'pressure',7x,'temperature'                   &
                ,/,/,1x,130('-'))")
        End If


        Write(nrite,"(4x,i10,2x,f12.3,6x,6(1x,e14.6))")nstep,time,             &
                                           stpval(4),stpval(1),stpval(2),      &
                                           stpval(3),stpval(5),stpval(6)

        Write(nrite,"(16x,f12.3,6x,6(1x,e14.6))")   timelp,                    &
                                           ravval(4),ravval(1),ravval(2),      &
                                           ravval(3),ravval(5),ravval(6)

        Write(nrite,"(1x,130('-'))")

! report end of equilibration period

        If (nstep == nsteql) Then
           Write(nrite,"(/,a,i8,/)")'equilibration period ended at step',nstep
           Write(nrite,"(1x,130('-'))")
        End If


        Write(*,"(1x,70('-'),/,/,                                              &
             15x,'step',5x,'time (dpd_sec)',5x,'cpu (sec)',                    &
              5x,'volume',/)")

        Write(*,"(9x,i10,2x,f12.3,6x,f12.3,2x,1p,e16.8)")                      &
                nstep,time,timelp,volm

!         Write(*,"(1x,70('-'))")

     End If

! ! ! ! !      If ((nstep > nsteql) .and. (Mod(nstep-nsteql,10000) == 0))                &
! ! ! ! !                          Call vd_profile(1,nstep-nsteql,10000)
! ! ! ! !      If (nstep > nsteql) Call vd_profile(0,nstep,10000)

! Save correlation function data every iscorr steps

     If (lcorr .and. (nstep > nsteql) .and. (Mod(nstep-nsteql,iscorr) == 0))   &
        Call corout(time)

! Write trajectory data

     If (ltraj) Call trajectory_write                                          &
                     (imcon,keyres,ndump,nstraj,istraj,nstep,tstep,time)

! Save restart data in event of system crash

     If (Mod(nstep,ndump) == 0 .and. nstep /= (nstart+nstrun))                 &
        Call system_restart                                                    &
             (imcon,rcut,lrdf,nstep,tstep,time,tmst,stress)

  End Do

  nstart=nstart+current_ts
  dnstep=nstep

End Subroutine time_integration

!***********************************************************************

Subroutine vv (isw,tstep,strkin,engke)

!    %--------------------------------------------------------------%
!    |  subroutine for solving the equations of motion based on     |
!    |  the velocity verlet algorithm.                              |
!    |                                                              |
!    |  Reference:                                                  |
!    |            Swope et al., j. chem. phys. 76, 637, 1982.       |
!    %--------------------------------------------------------------%


  Use kinds_f90
  Use setup_module
  Use config_module,      Only : natms,weight,lstfrz,                          &
                                 xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz

  Implicit None

  Integer,           Intent( In    ) :: isw
  Real( Kind = wp ), Intent( In    ) :: tstep
  Real( Kind = wp ), Intent( InOut ) :: strkin(1:9),engke


  Integer                            :: fail(1:1),i
  Real( Kind = wp )                  :: hstep,tmp

  ! timestep derivatives
  hstep = 0.5_wp*tstep

! first stage of velocity verlet algorithm

  If (isw == 0) Then

!    %---------------------------------------------------------------------------%
!    |  First part of velocity verlet algorithm                                  |
!    |                                                                           |
!    |  usage:                                                                   |
!    |        the first part of the algorithm is a taylor series which advances  |
!    |        positions from t to t + dt and velocities from t to t + dt/2.      |
!    |        after this, the force routine is called.                           |
!    %---------------------------------------------------------------------------%

     ! store initial values
     Do i=1, natms

        If (lstfrz(i) /= 0) Cycle
        tmp=hstep/weight(i)
        vxx(i)=vxx(i)+tmp*fxx(i)
        vyy(i)=vyy(i)+tmp*fyy(i)
        vzz(i)=vzz(i)+tmp*fzz(i)

        xxx(i)=xxx(i)+tstep*vxx(i)
        yyy(i)=yyy(i)+tstep*vyy(i)
        zzz(i)=zzz(i)+tstep*vzz(i)

     End Do

! second stage of velocity verlet algorithm

  Else

!    %-------------------------------------------------------------------%
!    |  Second part of velocity verlet algorithm                         |
!    |                                                                   |
!    |  usage:                                                           |
!    |        the second part of the algorithm advances velocities from  |
!    |        t + dt/2 to t + dt. this assumes that forces have been     |
!    |        computed in the force routine and stored in fxx, fyy, fzz. |
!    %-------------------------------------------------------------------%

     ! update velocity
     Do i=1, natms

        If (lstfrz(i) /= 0) Cycle
        tmp=hstep/weight(i)
        vxx(i)=vxx(i)+tmp*fxx(i)
        vyy(i)=vyy(i)+tmp*fyy(i)
        vzz(i)=vzz(i)+tmp*fzz(i)

     End Do

     ! set kinetic energy accumulator
     engke = 0.0_wp

     ! update kinetic energy
     Call kinstress(natms,weight,vxx,vyy,vzz,strkin)

     engke = 0.5_wp*(strkin(1)+strkin(5)+strkin(9))

  End If

End Subroutine vv


!***********************************************************************

Subroutine vv_m (isw,tstep,strkin,engke)

!    %-----------------------------------------------------------------%
!    |  subroutine for solving the equations of motion based on        |
!    |  the modified version of velocity verlet algorithm.             |
!    |                                                                 |
!    |  Reference:                                                     |
!    |            Groot & Warren, j. chem. phys. 107 (11) 15 Sep 1997  |
!    %-----------------------------------------------------------------%


  Use kinds_f90
  Use setup_module
  Use config_module,      Only : natms,weight,lstfrz,                          &
                                 xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz

  Implicit None

  Integer,           Intent( In    )   :: isw
  Real( Kind = wp ), Intent( In    )   :: tstep
  Real( Kind = wp ), Intent( InOut )   :: strkin(1:9),engke


  Integer                              :: fail(1:2),i
  Real( Kind = wp )                    :: hstep,tmp,lstep,tmpm,lambda

  Real( Kind = wp ), Allocatable, Save :: vxt(:),vyt(:),vzt(:)
  Real( Kind = wp ), Allocatable, Save :: fxt(:),fyt(:),fzt(:)

  fail=0
  lambda = 0.65_wp

! timestep derivatives

  hstep = 0.5_wp*tstep
  lstep = lambda*tstep

! first stage of velocity verlet algorithm

  If (isw == 0) Then

!    %---------------------------------------------------------------------------%
!    |  First part of modified version of velocity verlet algorithm              |
!    |                                                                           |
!    |  usage:                                                                   |
!    |        the first part of the algorithm is a taylor series which advances  |
!    |        positions from t to t + dt and velocities from t to t + dt/2.      |
!    |        after this, the force routine is called.                           |
!    %---------------------------------------------------------------------------%

     Allocate (vxt(1:natms),vyt(1:natms),vzt(1:natms), Stat=fail(1))
     Allocate (fxt(0:natms),fyt(0:natms),fzt(0:natms), Stat=fail(2))

     If (Any(fail > 0)) Then
        Write(nrite,'(/,1x,a)') 'vv_m allocation failure'
        Call error(0)
     End If

! store initial values

     Do i=1,natms

        If (lstfrz(i) /= 0) Cycle
        vxt(i) = vxx(i)   ;   fxt(i) = fxx(i)
        vyt(i) = vyy(i)   ;   fyt(i) = fyy(i)
        vzt(i) = vzz(i)   ;   fzt(i) = fzz(i)

     End Do

! update velocity and position

     Do i=1,natms

        If (lstfrz(i) /= 0) Cycle
        tmp=tstep*hstep/weight(i)
        tmpm=lstep/weight(i)

        xxx(i)=xxx(i)+tstep*vxt(i)+tmp*fxx(i)
        yyy(i)=yyy(i)+tstep*vyt(i)+tmp*fyy(i)
        zzz(i)=zzz(i)+tstep*vzt(i)+tmp*fzz(i)

        vxx(i)=vxt(i)+tmpm*fxx(i)
        vyy(i)=vyt(i)+tmpm*fyy(i)
        vzz(i)=vzt(i)+tmpm*fzz(i)

     End Do


! second stage of velocity verlet algorithm

  Else

!    %-------------------------------------------------------------------%
!    |  Second part of modified version of velocity verlet algorithm     |
!    |                                                                   |
!    |  usage:                                                           |
!    |        the second part of the algorithm advances velocities from  |
!    |        t + dt/2 to t + dt. this assumes that forces have been     |
!    |        computed in the force routine and stored in fxx, fyy, fzz. |
!    %-------------------------------------------------------------------%

! update velocity

     Do i=1,natms

        If (lstfrz(i) /= 0) Cycle
        tmp=hstep/weight(i)

        vxx(i)=vxt(i)+tmp*(fxt(i)+fxx(i))
        vyy(i)=vyt(i)+tmp*(fyt(i)+fyy(i))
        vzz(i)=vzt(i)+tmp*(fzt(i)+fzz(i))

     End Do

! set kinetic energy accumulator
     engke = 0.0_wp

! update kinetic energy

     Call kinstress(natms,weight,vxx,vyy,vzz,strkin)

     engke = 0.5_wp*(strkin(1)+strkin(5)+strkin(9))

     Deallocate (vxt,vyt,vzt,         Stat=fail(1))
     Deallocate (fxt,fyt,fzt,         Stat=fail(2))

     If (Any(fail > 0)) Then
        Write(nrite,'(/,1x,a)') 'vv_m deallocation failure'
        Call error(0)
     End If

  End If

End Subroutine vv_m

!***********************************************************************
