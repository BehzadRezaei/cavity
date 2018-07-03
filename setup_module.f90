Module setup_module

!    %----------------------------------------------------------------------------------%
!    |                                                                                  |
!    |  MAIN insert module setting fundamental parameters for compile time and          |
!    |  specifing execution parameters set @ execution time                             |
!    |                                                                                  |
!    |  Note(1): The following internal units apply everywhere                          |
!    |                                                                                  |
!    |                                                                                  |
!    |  unit of length (L)  = DPD_length (is the unit used to specify the particle size)|
!    |  unit of mass   (M)  = DPD_mass (is the unit used to specify the particle mass)  |
!    |  unit of energy (E)  = (is the unit used to define the pair potential)           |
!    |  unit of time   (t)  = DPD_seconds (the unit of time t = L*(M/E)**0.5)           |
!    |  unit of temperature = (T=(2/3)*KE where KE is kinetic energy per particle       |
!    |                         in units of E )                                          |
!    |  unit of pressure    =                                                           |
!    |                                                                                  |
!    |                                                                                  |
!    |                                                                                  |
!    |  Note(2): All modules, defining (and allocating) module specific                 |
!    |           variables, MUST initialize ALL of THEM to ZERO                         |
!    |                                                                                  |
!    %----------------------------------------------------------------------------------%

  Use kinds_f90

  Implicit None


! FIXED PARAMETERS
! standard pi values

  Real( Kind = wp ), Parameter ::    pi = 3.1415926535897932e0_wp
  Real( Kind = wp ), Parameter :: sqrpi = 1.7724538509055160e0_wp

! standard square roots

  Real( Kind = wp ), Parameter :: rt2 = 1.4142135662373095e0_wp
  Real( Kind = wp ), Parameter :: rt3 = 1.7320508075688772e0_wp

! boltzmann constant

  Real( Kind = wp ), Parameter :: boltz = 8.31451115e-1_wp*1.660540200e-23_wp

!boltz=1.380658000792323e-23_wp  j/k =8.31451115e-1_wp*1.660540200e-23_wp

! conversion factor for pressure from internal units to katm

  Real( Kind = wp ), Parameter :: prsunt = 1.0_wp

! I/O CHANNELS
! main input channel

  Integer, Parameter :: nread = 7

! main output channel

  Integer, Parameter :: nrite = 10

! force field input channel

  Integer, Parameter :: nfield = 15

! configuration file input channel

  Integer, Parameter :: nconf = 20

! trajectory history file channel

  Integer, Parameter :: nhist = 21

! statistical data file output channel

  Integer, Parameter :: nstats = 25

! x-density file channel number

  Integer, Parameter :: nxdndt = 26

! y-density file channel number

  Integer, Parameter :: nydndt = 27

! z-density file channel number

  Integer, Parameter :: nzdndt = 28

! rdf file channel number

  Integer, Parameter :: nrdfdt = 30

! acummulators restart dump file

  Integer, Parameter :: nrest =  35

! correlation data file output channel

  Integer, Parameter :: ncorr =  40

! making output configuration

  Integer, Parameter :: ncout = 45


  Integer, Parameter :: nvout = 50
  Integer, Parameter :: ndout = 55
  Integer, Parameter :: nhout = 60
  Integer, Parameter :: nverout = 65

! GLOBAL PARAMETERS FOR ARRAYS' BOUNDS LIMITS (set_bounds)

  Integer,           Save :: mxlist,mxcell,mxatms,mxgrdf,mxrdf,  &
                             mxatyp,mxstak,mxnstk,mxpot,mxpfld

! zero+ and half-

  Real( Kind = wp )       :: zero_plus,half_minus,half_plus


End Module setup_module

