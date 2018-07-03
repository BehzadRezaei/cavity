!    %-----------------------------------------------------------------%
!    |  D P D  --  version 2.0    /    April    2010     --    y.afshar
!    |             ya_afshar@me.iut.ac.ir
!    %-----------------------------------------------------------------%

!    %-----------------------------------------------------------------%
!    | module for accumulating & calculating statistics
!    | for calculation of viscosity
!    %-----------------------------------------------------------------%

Module viscosity_module

  Use kinds_f90
  Use setup_module
  Use param_module, Only : shrate

  Implicit None

  Integer, Save :: numvs
  Data             numvs / 0 /

  Real( kind = wp ), Save :: tzx
  Data                       tzx / 0.0_wp /

  Real( Kind = wp ) :: viscosity, tawzx

  Private
  Public :: viscosity_collect, viscosity_compute,                              &
            numvs

Contains

  !    %-----------------------------------------------------------------%
  !    | subroutine for accumulating statistic for viscosity computing
  !    %-----------------------------------------------------------------%

  Subroutine viscosity_collect(nstep, stress)

    !    %-----------------------------------------------------------------%
    !    | author             - y.afshar         February    2010
    !    %-----------------------------------------------------------------%

    Use config_module,     Only : natms, lstfrz, xxx, yyy, zzz,                &
                                  weight, volm,  vxx, vyy, vzz

    Implicit None

    Integer,           Intent( In    ) :: nstep
    Real( Kind = wp ), Intent( In    ) :: stress(1:9)

    Integer :: i
    Real( kind = wp ) :: strkin, tmp

    strkin=0.0_wp
    tmp=0.0_wp

    numvs=numvs+1

    Do i=1, natms
       If (lstfrz(i) == 0) strkin=strkin + weight(i)*(vxx(i)-zzz(i)*shrate)*vzz(i)
!        If (lstfrz(i) == 0) strkin=strkin + weight(i)*vxx(i)*vzz(i)
    End Do

    tmp=-(strkin-stress(3))/volm
    tzx=tzx+tmp

    If (Mod(nstep, 1000) == 0) Call viscosity_compute(nstep)

  End Subroutine viscosity_collect

  !    %-----------------------------------------------------------------%
  !    | subroutine for calculating viscosity from accumulated data
  !    %-----------------------------------------------------------------%

  Subroutine viscosity_compute(nstep)

    !    %-----------------------------------------------------------------%
    !    | author             - y.afshar         February    2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Integer, Intent( In    ) :: nstep

    tawzx=tzx/Real(numvs, Kind=wp)
    viscosity=tawzx/shrate

    Write(*,"(/,/,1x,a,i10,2(2x,a,e14.6))")                                    &
         'step =', nstep,', tawzx =', tawzx,', dynamic_viscosity =', viscosity

    Write(nrite,"(/,/,1x,a,1x,1p,e14.6,0p,1x,a,i12,2x,a,/,/)")                 &
         'Approximate dynamic viscosity of the DPD fluid =', viscosity,        &
         'Calculated over ',numvs,'steps'
    Write(nrite,"(1x,130('-'))")

  End Subroutine viscosity_compute

End Module viscosity_module