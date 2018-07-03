!    %-----------------------------------------------------------------%
!    |  D P D  --  version 1.4    /    December  2008  --   y.afshar   |
!    %-----------------------------------------------------------------%

Module param_module

  !    %------------------------------------------------------------------------%
  !    | module declaring file for parameters and costants used in the entire   |
  !    | DPD packages                                                           |
  !    %------------------------------------------------------------------------%

  Use kinds_f90

  Implicit None

  ! Nearest neighbor particle searching (nnps) method
  !
  ! nnps = 1                      : simplest and direct searching
  !        2                      : sorting grid linked list (Monaghan)
  !        3                      : verlet neighbour list based on link-cell method
  Integer,           Parameter :: nnps=3

  ! Internal Restart Key : keyres meaning

  ! 0 start new simulation
  ! 1 continue current simulation
  Integer,                Save :: keyres = 0

!------------------------BOUNDARY CONDITION---------------------------
! Set boundary condition
!
!                 y
!                  \   |z
!                    \ |
!                       ----x
!
!
! imcon=0 no specified boundary conditions apply
! imcon=1 orthorhombic boundaries apply          !Periodic1
! imcon=2 Slab (x,y periodic, z non-periodic)
! imcon=3 2D (y periodic, x,z non-periodic)
! imcon=4 Lees Edwards (x,y periodic, z uniform shear)
! imcon=5
! imcon=6
! imcon=7

  Integer,           Parameter :: imcon = 1

!---------------------------------------------------------------------

  ! ex_force =.True.              : Consider external force,
  !           .False.             : No external force.
  Logical,           Parameter :: ex_force = .False.

  ! rdf calculation option

  ! default switch for calculation of rdfs,
  ! default number of steps when to be collected
  ! key to the number of specified rdf look up pairs
  Logical,           Parameter :: lrdf = .False.
  Integer,                Save :: nstrdf = 10
  Integer, Allocatable,   Save :: keyrdf(:)

  ! default switch for calculation of density,
  ! default number of steps when to be collected
  ! default number of steps when to be averaged over
  Logical,           Parameter :: lden = .False.
  Integer,                Save :: nstden = 10
  Integer,                Save :: istden = 10000

  ! set the bin size for radial distribution functions
  Real( Kind = wp ),      Save :: rbin = 0.05_wp


! CONTROL PARAMETERS FOR STARTING THE PROGRAM


  ! cutoff radious, rcut in DPD units
  Real( Kind = wp ), Parameter :: rcut = 1.0_wp

  ! twobody interactions cutoff radious
  ! this is the radious that will be used for
  ! two body interactions in DPD force computing
  Real( Kind = wp ), Parameter :: rvdw = 1.0_wp

  ! temperature default value
  Real( Kind = wp ), Parameter :: kt = 1.0_wp

  ! temperature switche and temperature scaling interval
  Logical,                Save :: ltemp
  Data                            ltemp / .True. /
  Integer,           Parameter :: nstscal = 10

!-----------------------------potential---------------------------------------/
! potential function key                                                      |
!                                                                             |
! ktype=1   -->   espanol and warren potential                                |
!                                                                             |
  Integer,                  Parameter :: ktype = 1                            !
!                                                                             |
! potential parameter                                                         |
  Real( Kind = wp ),Allocatable, save :: aaa(:),bbb(:)                        !
!                                                                             |
!                                                                             |
! Multi component DPD fluid has different aij between different particles     |
!                                                                             |
  Real( Kind = wp ),Allocatable, Save :: vvv(:,:)                             !
!-----------------------------------------------------------------------------/


  ! vatms_(x,y,z) number For Virtual particles On the wall (natms_x * )
  Integer,           Parameter :: natms_x = 10 , vatms_x = Int(natms_x*2.0_wp)
  Integer,           Parameter :: natms_y = 30 , vatms_y = Int(natms_y*1.0_wp)
  Integer,           Parameter :: natms_z = 10 , vatms_z = Int(natms_z*2.0_wp)


  ! Integration flavour
  ! l_vv=0 Velocity Verlet Algorithm
  ! l_vv=1 Modified Version of Velocity Verlet Algorithm
  Integer,           Parameter :: l_vv = 1

  ! timestep and times for job execution
  Real( Kind = wp ),      Save :: tstep = 0.01_wp
  Real( Kind = wp ),      Save :: timjob = 0.0_wp

  ! total number of steps to run
  ! default total number steps to run
  Integer,                Save :: nstrun  = 0
  Integer,                Save :: nstrund = 100001

  ! number of steps for equilbration
  Integer,                Save :: nsteql = 3000

  ! If you are using imcon=4 then you should give the shear rate
  ! defult number of steps for viscosity to start calculating
  ! at (nstep = nsteql + nstvis)
  Real( Kind = wp ), Parameter :: shrate = 0.14_wp
  Integer,           Parameter :: nstvis = 10000

  ! default for statistics file interval
  Integer,                Save :: intsta = 100

  ! restart options
  ! default value for data dumping interval
  Integer,                Save :: ndump = 100000

  ! default switch for writing output configuration in write_config
  ! at ndump interval when writing RESTART file
  Logical,           Parameter :: lconf = .False.

  ! default for data correlation printing
  Logical,           Parameter :: lcorr  = .True.
  Integer,                Save :: iscorr = 100

  ! default switch for configuration outputing and defaults for
  ! (i) step to start at, (ii) every step after to be collected,
  Logical,           Parameter :: lcout  = .True.
  Integer,                Save :: nstout = 3000
  Integer,                Save :: istout = 2500


  ! default switch for trajectory outputing and defaults for
  ! (i) step to start at, (ii) every step after to be collected,
  Logical,           Parameter :: ltraj  = .False.
  Integer,                Save :: nstraj = 1
  Integer,                Save :: istraj = 1


! CONTROL PARAMETERS FOR OUTPUT


  ! default for data printing interval
  ! print_step                    : Print Timestep  (On Screen)
  ! save_step                     : Save Timestep   (To Disk File)
  Integer,           Parameter :: print_step = 100
  Integer,           Parameter :: save_step  = 100


! SIMULATION CASES


  ! config_input = .TRUE.       : Load initial configuration data,
  !               .FALSE.       : Generate initial configuration.
  Logical,           Parameter :: config_input = .False.

  ! shearcavity = .True.        : carry out shear cavity simulation
  Logical,           Parameter :: shearcavity = .True.
  Logical,           Parameter :: Periodic1   = .False.
  Logical,           Parameter :: Periodic2   = .False.


! GEOMETRY


  ! properies of a simulation cell specified by the input cell
  ! vectors in rows, the matrix is in the form of one dimensional
  ! reading (row1,row2,row3).
  Real( Kind = wp ), Parameter :: x1_geom = 10.00_wp,  y1_geom =  0.00_wp,  z1_geom =  0.00_wp
  Real( Kind = wp ), Parameter :: x2_geom =  0.00_wp,  y2_geom = 10.00_wp,  z2_geom =  0.00_wp
  Real( Kind = wp ), Parameter :: x3_geom =  0.00_wp,  y3_geom =  0.00_wp,  z3_geom = 10.00_wp

  !
  !       (xx3,yy3,zz3)
  !             |
  !             |               (xx2,yy2,zz2)
  !             |                 /
  !             |               /
  !             |             /
  !   (x3_g,y3_g,z3_g)      / (x2_g,y2_g,z2_g)
  !             |         /
  !             |       /
  !             |     /
  !             |   /
  !               /
  !      (x,y,z). -----------------------(xx1,yy1,zz1)
  !                  (x1_g,y1_g,z1_g)
  !

  Real( Kind = wp ),      Save :: aij,gamma,sigma

  Public :: allocate_param_arrays

Contains

  Subroutine allocate_param_arrays()

    Use kinds_f90
    Use setup_module
    Use statistics_module,     Only : lstrdf, ntprdf
    Use external_field_module

    Implicit None

    Integer, Dimension( 1:3 ) :: fail
    Integer, Allocatable      :: atmrdf1(:),atmrdf2(:)
    Integer                   :: i,j,k
    Real( kind=wp )           :: aak,bbk

    ! external field data
    If (ex_force) Then

       keyfld = 5
       mxpfld = 1
       Call allocate_external_field_arrays()
       prmfld(1) = 0.055_wp

    End If

     ! max number of different particle types, It depends on the problem
     mxatyp = 1
     mxpot = 0
     mxpot = Max(mxpot,(mxatyp*(mxatyp+1))/2)

     ! data stacking interval
     ! dimension of stack arrays for rolling averages
     ! maximum number of timesteps in stack arrays
     ! The maximum number of time-steps used to calculate the rolling averages
     mxstak = 100

     ! maximum number of variables in stack arrays
     mxnstk = 6 + 9 + mxatyp

     ! set maximum dimension of rdf
     ! mxgrdf::number of grid points for each RDF pair
     mxgrdf = Nint(rcut/rbin)

     ! number of maximum possible radial distribution pairs
     mxrdf=0
     mxrdf=Max(mxrdf,(mxatyp*(mxatyp+1))/2)

     ! number of specified rdf look up pairs
     ntprdf = 1

     fail=0
     Allocate (keyrdf(1:mxrdf),                                Stat = fail(1))
     Allocate (atmrdf1(1:ntprdf),atmrdf2(1:ntprdf),            Stat = fail(2))
     Allocate (vvv(1:2,1:mxpot),aaa(1:mxatyp),bbb(1:mxatyp),   Stat = fail(3))

     keyrdf=0

     atmrdf1=0
     atmrdf2=0

     vvv=0.0_wp
     aaa=0.0_wp
     bbb=0.0_wp

     If (Any(fail > 0)) Call error(105)

     ! It depends on your choice of atom type you should
     ! consider you should add the pairs you wnat to
     ! calculate radial distribution function for them


     !        type of DPD particle            type of DPD particle
     !               -----                           -----
     atmrdf1(1)       = 1      ;      atmrdf2(1)      = 1      !pair one
!    atmrdf1(2)       = 3      ;      atmrdf2(2)      = 2      !pair two
!    atmrdf1(3)       = 1      ;      atmrdf2(3)      = 1      !pair three
!    ....................          ....................        ............
!    ....................          ....................        ............
!    ....................          ....................        ............
!    ....................          ....................        ............
!    ....................          ....................        ............
!    atmrdf1(ntprdf)  = 1      ;    atmrdf2(ntprdf)   = 1      !pair ntprdf

     Do i=1,ntprdf
        keyrdf(i)=(Max(atmrdf1(i),atmrdf2(i))*                                 &
                  (Max(atmrdf1(i),atmrdf2(i))-1))/2+Min(atmrdf1(i),atmrdf2(i))
        If (keyrdf(i) > mxrdf) Call error(109)
     End Do


     ! potential parameter aaa,bbb
     aij = 25.0_wp           ;         gamma = 4.5_wp
     sigma = (2.0_wp*gamma*kt)**0.5_wp

     Select Case (Size(aaa, Dim=1))

        ! one particle type
        Case( 1)
           aaa( 1) = aij            ;         bbb( 1) = gamma
        ! two types of particles
        Case( 2)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
        ! three types of particles
        Case( 3)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
        ! four types of particles
        Case( 4)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
        ! five types of particles
        Case( 5)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
           aaa( 5) = aij            ;         bbb( 5) = gamma
        ! six types of particles
        Case( 6)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
           aaa( 5) = aij            ;         bbb( 5) = gamma
           aaa( 6) = aij            ;         bbb( 6) = gamma
        ! seven types of particles
        Case( 7)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
           aaa( 5) = aij            ;         bbb( 5) = gamma
           aaa( 6) = aij            ;         bbb( 6) = gamma
           aaa( 7) = aij            ;         bbb( 7) = gamma
        ! eight types of particles
        Case( 8)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
           aaa( 5) = aij            ;         bbb( 5) = gamma
           aaa( 6) = aij            ;         bbb( 6) = gamma
           aaa( 7) = aij            ;         bbb( 7) = gamma
           aaa( 8) = aij            ;         bbb( 8) = gamma
        ! nine types of particles
        Case( 9)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
           aaa( 5) = aij            ;         bbb( 5) = gamma
           aaa( 6) = aij            ;         bbb( 6) = gamma
           aaa( 7) = aij            ;         bbb( 7) = gamma
           aaa( 8) = aij            ;         bbb( 8) = gamma
           aaa( 9) = aij            ;         bbb( 9) = gamma
        ! ten types of particles
        Case(10)
           aaa( 1) = aij            ;         bbb( 1) = gamma
           aaa( 2) = aij            ;         bbb( 2) = gamma
           aaa( 3) = aij            ;         bbb( 3) = gamma
           aaa( 4) = aij            ;         bbb( 4) = gamma
           aaa( 5) = aij            ;         bbb( 5) = gamma
           aaa( 6) = aij            ;         bbb( 6) = gamma
           aaa( 7) = aij            ;         bbb( 7) = gamma
           aaa( 8) = aij            ;         bbb( 8) = gamma
           aaa( 9) = aij            ;         bbb( 9) = gamma
           aaa(10) = aij            ;         bbb(10) = gamma
        Case Default
           Write(nrite, '(a)')'# error: There are more than ten types of particles in the simulation'
           Call error(0)

     End Select

     Do i=1,mxatyp
        Do j=i,mxatyp

           k=j*(j-1)/2+i

           If (ktype == 1) Then
              aak=Sqrt(aaa(i)*aaa(j))
              bbk=Sqrt(bbb(i)*bbb(j))
!               If(i /= j)aak=37.0_wp
              vvv(1,k)=aak
              vvv(2,k)=bbk
           End If

        End Do
     End DO

     Deallocate(atmrdf1,atmrdf2,  Stat = fail(1))
     If (Any(fail > 0)) Call error(0)

  End Subroutine allocate_param_arrays

End Module param_module