!    %-----------------------------------------------------------------%
!    |  D P D  --  version 1.4    /    December  2008  --   y.afshar   |
!    %-----------------------------------------------------------------%

Program dpd

! SETUP MODULES

  Use kinds_f90
  Use setup_module
  Use param_module

! CONFIG MODULES

  Use config_module

! STATTISTICS MODULE

  Use statistics_module

! DENSITY MODULE

  Use density_module


! MAIN PROGRAM VARIABLES

  Implicit None

! newjob used for trajectory_write
  Real( Kind = wp )  :: timelp, tmst, time, engke,                  &
                        strkin(1:9), press, stress(1:9)

  Integer :: i, nstep, maxtimestep, yesorno

  Logical :: safe1, safe2

  Open(nrite, File = 'OUTPUT')

  Write(nrite,'(18(15x,a,/))')                                       &
       "****************************************",                   &
       "****************************************",                   &
       "*                                      *",                   &
       "*                D P D                 *",                   &
       "*                                      *",                   &
       "*  version 1.0    /    September 2008  *",                   &
       "*  version 1.4    /    December  2008  *",                   &
       "*                                      *",                   &
       "*             y.afshar                 *",                   &
       "*                                      *",                   &
       "*                                      *",                   &
       "* Department of Mechanical Engineering *",                   &
       "* Isfahan University of Technology     *",                   &
       "*                                      *",                   &
       "* Email: ya_afshar@me.iut.ac.ir        *",                   &
       "*                                      *",                   &
       "****************************************",                   &
       "****************************************"

! SET UP CLOCKING
  Call gtime(timelp)

! DETERMINE ARRAYS' BOUNDS LIMITS
  Call set_bounds()

! ALLOCATE PARAMETER ARRAYS
  Call allocate_param_arrays()

! ALLOCATE CONFIG ARRAYS
  Call allocate_config_arrays()

! ALLOCATE STATISTICS ARRAYS
  Call allocate_statistics_arrays()

! ALLOCATE DENSITY ARRAYS
  If (lden) Call allocate_density_arrays()

  If (keyres == 0) Then

     If (config_input) Then
        Inquire(File='RESCONFIG', Exist=safe1)
        Inquire(File='CONFIG',    Exist=safe2)
        If (safe1) Then
           Call input
        Else If (safe2) Then
           Call hread ('CONFIG')
        End If
     Else If (Periodic1) Then
        Call Orthorhombic
     Else If (Periodic2) Then
        Call Orthorhombic2
     Else If (shearcavity) Then
        Call cavity
     Else
        Call error(0)
     End if

  End If

! Read RESTART (thermodynamic and structural data from restart file)
  Call system_init                                                   &
       (imcon,rcut,lrdf,keyres,time,tmst,nstep,stress)

! Set initial system temperature
  Call set_temperature(imcon,kt,keyres)

! Print out sample of initial configuration

  Write(nrite,"(/,/,1x,'sample of starting configuration',/)")
  Write(nrite,"(8x,'i',7x,'x(i)',8x,'y(i)',8x,'z(i)',                &
                7x,'vx(i)',7x,'vy(i)',7x,'vz(i)',                    &
                7x,'fx(i)',7x,'fy(i)',7x,'fz(i)',/,/)")

  Do i=1,natms,(natms+19)/20

     Write(nrite,"(1x,i8,1x,1p,3e12.4,3e12.4,3e12.4)")               &
           i,xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i),              &
                                  fxx(i),fyy(i),fzz(i)

  End Do

1 Write(*,'(/,/,5(10x,a,/))')                                        &
       '  ***************************************************',      &
       '  *       Please input maximal time steps           *',      &
       '  *                                                 *',      &
       "  *       (Enter '0' for Using Default value)       *",      &
       '  ***************************************************'
  Write(*,'(/,10x,a,$)')' maxtimestep = '
  Read(*,*) maxtimestep
  Write(*,'(/)')

  Call gtime(timelp)

  Write(nrite,'(/,/,/,1x,                                            &
       "time elapsed since job start :",f12.3, " sec",/)') timelp

!  %----------------------------%
!  |  Start of DPD calculations |
!  %----------------------------%

  Call time_integration(maxtimestep,time,tmst,nstep)

!  %----------------------------%
!  |  End of DPD calculations   |
!  %----------------------------%


  Write(*,'(/,/,3(10x,a,/))')                                       &
       '  ***************************************************',     &
       '  *       Are you going to run more time steps ? ',         &
       '  ***************************************************'
  Write(*,'(10x,a,$)')'  (0=No, 1=Yes) : '
  Read(*,*) yesorno
  Write(*,'(/,/)')
  If (yesorno == 1) Goto 1

! Report terminationg of the DPD simulation
  Call gtime(timelp)

  Write(nrite,"(/,/,1x,'run terminating....  ',                     &
        'elapsed cpu time: ', f12.3, ' sec,     job time: ',        &
         f12.3,' dpd_sec',/)") timelp,timjob

! Save restart data (final)

  Call system_restart                                               &
       (imcon,rcut,lrdf,nstep,tstep,time,tmst,stress)

! Produce summary of simulation

  Call statistics_result                                            &
       (rcut,lrdf,nstep,tstep,time,tmst)

  Close(Unit=nrite)
  If (ltraj) Close(Unit=nhist)
  If (lcorr) Close(Unit=ncorr)

End Program dpd