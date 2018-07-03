!    %-----------------------------------------------------------------%
!    |  D P D  --  version 2.0    /    December  2009  --   y.afshar
!    |             ya_afshar@me.iut.ac.ir
!    %-----------------------------------------------------------------%

Module random_module

  !    %-----------------------------------------------------------------%
  !    | This module contains the random number generator and related
  !    | subroutines.
  !    | There are three random number generators coded in here:
  !    |
  !    |  1.) uni :
  !    |
  !    | This random number generator originally appeared in "Toward a
  !    | Universal Random Number Generator" by George Marsaglia, Arif Zaman and
  !    | W.W. Tsang in Florida State University Report: FSU-SCRI-87-50 (1987).
  !    | It was later modified by F. James and published in "A Review of
  !    | Pseudo-random Number Generators".
  !    | THIS IS THE BEST KNOWN RANDOM NUMBER GENERATOR AVAILABLE.
  !    | It passes ALL of the tests for random number generators and has a
  !    | period of 2^144, is completely portable (gives bit identical results
  !    | on all machines with at least 24-bit mantissas in the floating point
  !    | representation).
  !    | Use iseed1 = 1802 & iseed2 = 9373 to test the random number
  !    | generator. Using it to generate 20000 random numbers.
  !    | Then display the next six random numbers generated multiplied by 4096*4096.
  !    | If the random number generator is working properly, the random numbers should be:
  !    |
  !    | 6533892.0                   14220222.0                  7275067.0
  !    | 6172232.0                   8354498.0                   10633180.0
  !    |
  !    |
  !    |  2.) uni64 :
  !    |
  !    | This is an extension to 64-bit floating point format of the ‘universal’
  !    | random number generator (RNG) of Marsaglia, et al. (1990).
  !    | The RNG, and its extension here, are designed to produce uniform [0,1)
  !    | random variables directly, without the usual floating of integers,
  !    | to have a very long period, and to be such that exactly the same numbers
  !    | could be produced on most of the common platforms.
  !    | It will generate 64-bit uniform [0,1) random variables using
  !    | the IEEE 754 standard.
  !    | The ±0 may appear, about once in some 9 × 10^15 times , but will
  !    | cause no problem.
  !    | It passes ALL of the tests for random number generators and has a
  !    | period of 2^202.
  !    | Use iseed1 = 123456789 & iseed2 = 987654321 to test the
  !    | random number generator. After using it to generate 10000000
  !    | random numbers. Then display the next five random numbers generated
  !    | multiplied by 9007199254740992.0.
  !    | If the random number generator is working properly, the random numbers should be:
  !    |
  !    | 4973478098168054.0          7093627369399858.0          6724709031495432.0
  !    | 3308798729116051.0          7384281049802113.0
  !    |
  !    |
  !    |  3.) unif :
  !    |
  !    | Taken from numerical recipes in fortran90: the art of
  !    | parallel scientific computing" W.H Press et al. See pages 1141-1143
  !    | in chapter B7. The book is online and free. The code is available at
  !    | "/fiesta/software/numerical.recipes/f90/recipes/ran.f90"
  !    |  It was modified to fit inot dpd's way of calling unif
  !    |
  !    |
  !    |
  !    | There are two gaussian random number generators coded in here:
  !    |
  !    |
  !    |  1.) kgauss :
  !    |
  !    | Taken from "The Art of Computer Programming", this
  !    | routine is for constructing a gaussian distribution in form :
  !    | - with mean  "0"  and standard deviation   "1".
  !    |
  !    |
  !    |  2.) gauss :
  !    |
  !    | These routines are using the box-muller method for generating gaussian
  !    | random numbers in two forms :
  !    | - with mean "ave" and standard deviation "sigma".
  !    | - with mean  "0"  and standard deviation   "1".
  !    |
  !    %-----------------------------------------------------------------%

  Use kinds_f90, Only: wp, ip

  Implicit None

  Private
  Public :: uni, uni64, unif, gauss, kgauss

  Type :: random_gen
     Sequence
     Real(Kind = wp) :: u(97)
     Real(Kind = wp) :: c
     Real(Kind = wp) :: d
     Real(Kind = wp) :: r
     Integer         :: i97
     Integer         :: j97
  End Type random_gen

  Integer, Parameter :: idnode=0

  Interface gauss
    Module Procedure Box_Muller_gaussian_1
    Module Procedure Box_Muller_gaussian_2
  End Interface

  Interface kgauss
    Module Procedure Knuth_random_gaussian
  End Interface

Contains

  !    %-----------------------------------------------------------------%
  !    | routine for setting the seeds for the random no. generator
  !    %-----------------------------------------------------------------%

  Subroutine random_init(iseed1, iseed2)

    !    %-----------------------------------------------------------------%
    !    | author           - y.afshar           January     2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Integer, Intent(  Out)           :: iseed1
    Integer, Intent(  Out), Optional :: iseed2

    Integer :: ih, ic, im

    ! First random number seed must be between 0 and 31328
    ! Second seed must have a value between 0 and 30081
    Call System_Clock(ih, ic, im)
    iseed1=Mod(ih+idnode, 31328)
    ih=Mod(im, ih)
    If (Present(iseed2)) iseed2=Mod(ih+idnode, 30081)

  End Subroutine random_init

  !    %-----------------------------------------------------------------%
  !    |  random number generator based on the universal random number
  !    |  generator of marsaglia, zaman and tsang
  !    |
  !    |  (stats and prob. lett. 8 (1990) 35-39.)
  !    %-----------------------------------------------------------------%

  Real(Kind = wp) Function uni()

    !    %-----------------------------------------------------------------%
    !    | author           - y.afshar           January     2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Type(random_gen), Save :: st

    Real(kind = wp) :: s, t

    Integer  :: iseed1, iseed2
    Integer  :: i, j, k, l, m
    Integer  :: ii, jj

    Logical, Save :: newjob
    Data             newjob /.True./

    If (newjob) Then
       newjob=.False.

       Call random_init(iseed1, iseed2)

       i = Mod(iseed1/177, 177) + 2
       j = Mod(iseed1    , 177) + 2
       k = Mod(iseed2/169, 178) + 1
       l = Mod(iseed2    , 169)

       Do ii = 1, 97

          s = 0.0_wp
          t = 0.5_wp

          Do jj = 1, 24

             m = Mod(Mod(i*j, 179)*k, 179)
             i = j
             j = k
             k = m
             l = Mod(53*l+1, 169)
             If (Mod(l*m, 64) >= 32) s = s + t
             t = 0.5_wp * t

          End Do
          st%u(ii) = s

       End Do

       st%c =   362436.0_wp / 16777216.0_wp
       st%d =  7654321.0_wp / 16777216.0_wp
       st%r = 16777213.0_wp / 16777216.0_wp

       st%i97 = 97
       st%j97 = 33

    End If

    uni = st%u(st%i97) - st%u(st%j97)
    If ( uni < 0.0_wp ) uni = uni + 1.0_wp
    st%u(st%i97) = uni
    st%i97 = st%i97 - 1
    If (st%i97 == 0) st%i97 = 97
    st%j97 = st%j97 - 1
    If (st%j97 == 0) st%j97 = 97
    st%c = st%c - st%d
    If ( st%c < 0.0_wp ) st%c = st%c + st%r
    uni = uni - st%c
    If ( uni < 0.0_wp ) uni = uni + 1.0_wp

  End Function uni

  !    %-----------------------------------------------------------------%
  !    |  random number generator based on the 64-bit universal RNG
  !    |  of marsaglia and tsang
  !    |
  !    |  (Statistics & Probability Letters 66 (2004) 183–187)
  !    %-----------------------------------------------------------------%

  Real(Kind = wp) Function uni64()

    !    %-----------------------------------------------------------------%
    !    | author           - y.afshar           January     2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Type(random_gen), Save :: st

    Real(kind = wp) :: s, t

    Integer  :: iseed1, iseed2
    Integer  :: i, j
    Integer  :: ii, jj

    Logical, Save :: newjob
    Data             newjob /.True./

    If (newjob) Then
       newjob=.False.

       Call random_init(iseed1, iseed2)

       i = iseed1
       j = iseed2

       Do ii = 1, 97

          s = 0.0_wp
          t = 0.5_wp

          Do jj = 1, 53

             i = Mod((6969*i), 65543)
             j = Mod((8888*i), 65579)
             If (Iand(Ieor(i, j), 32) >  0) s = s + t
             t = 0.5_wp * t

          End Do
          st%u(ii) = s

       End Do

       st%c =                 0.0_wp
       st%d =      362436069876.0_wp / 9007199254740992.0_wp
       st%r =  9007199254740881.0_wp / 9007199254740992.0_wp

       st%i97 = 97
       st%j97 = 33

    End If

    uni64 = st%u(st%i97) - st%u(st%j97)
    If ( uni64 < 0.0_wp ) uni64 = uni64 + 1.0_wp
    st%u(st%i97) = uni64
    st%i97 = st%i97 - 1
    If (st%i97 == 0) st%i97 = 97
    st%j97 = st%j97 - 1
    If (st%j97 == 0) st%j97 = 97
    st%c = st%c - st%d
    If ( st%c < 0.0_wp ) st%c = st%c + st%r
    uni64 = uni64 - st%c
    If ( uni64 < 0.0_wp ) uni64 = uni64 + 1.0_wp

  End Function uni64

  !    %-----------------------------------------------------------------%
  !    | Uniform Random Number Generator see header of the Module for
  !    | more details.
  !    |
  !    | The Function value is a Real number on the Open
  !    | interval (0,1) (0 and 1 not included)
  !    %-----------------------------------------------------------------%

  Real(kind = wp) Function unif()

    !    %-----------------------------------------------------------------%
    !    | author           - y.afshar           January     2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Real(kind = wp), Save :: am

    !** variables IA, IM, IQ and IR has been rename by adding "CONST_" prefix
    Integer(kind = ip), Parameter :: ia= 16807_ip, im=2147483647_ip
    Integer(kind = ip), Parameter :: iq=127773_ip, ir=2836_ip
    Integer(kind = ip)            :: k
    Integer                       :: iseed, ix=-1, iy=-1

    Logical, Save :: newjob
    Data             newjob / .True. /

    !** Initialize
    If (newjob) Then
      newjob=.False.
      Call random_init(iseed)
      am=Nearest(1.0, -1.0)/im
      iy=Ior(Ieor(888889999, Abs(iseed)), 1)
      ix=Ieor(777755555, Abs(iseed))
      iseed=Abs(iseed)+1
    End If

    !** The Marsaglia shift sequence with period of 2**32 -1
    ix=Ieor(ix, Ishft(ix,  13))
    ix=Ieor(ix, Ishft(ix, -17))
    ix=Ieor(ix, Ishft(ix,   5))

    !** The Park-Miller sequence by schrages's method. Period=2**31-2
    k=iy/iq
    iy=ia*(iy-k*iq)-ir*k
    if (iy < 0) iy=iy+im

    !** Combine both sequences (to improve randomness, I guess)
    !** Note that the combined sequence has period of 2**64
    unif=am*Ior(Iand(im, Ieor(ix, iy)), 1)

  End Function unif

  !    %-----------------------------------------------------------------%
  !    |  routine for constructing a gaussian distribution of
  !    |  unit variance (zero mean), based on the method described in
  !    |
  !    |  Knuth D, "The Art of Computer Programming",
  !    |  (2nd edition Addison-Wesley), 1978
  !    %-----------------------------------------------------------------%

  Real(Kind = wp) Function Knuth_random_gaussian()

    Implicit None

    Logical, Save ::newjob
    Data            newjob /.True./

    Real(Kind = wp), Parameter :: a1 = 3.949846138_wp
    Real(Kind = wp), Parameter :: a3 = 0.252408784_wp
    Real(Kind = wp), Parameter :: a5 = 0.076542912_wp
    Real(Kind = wp), Parameter :: a7 = 0.008355968_wp
    Real(Kind = wp), Parameter :: a9 = 0.029899776_wp

    Real(Kind = wp) :: rrr, rr2

    If (newjob) Then
       newjob=.False.

       rrr=uni()

    End If

    rrr=(uni()+uni()+uni()+uni()+uni()+uni()+uni()+uni()+uni()+uni()+uni()+uni()-6.0_wp)/4.0_wp
    rr2=rrr*rrr
    Knuth_random_gaussian=rrr*(a1+rr2*(a3+rr2*(a5+rr2*(a7+rr2*a9))))

  End Function Knuth_random_gaussian

  !    %-----------------------------------------------------------------%
  !    |  routine using the box-muller method for generating gaussian
  !    |  random numbers with mean "ave" and standard deviation "sigma".
  !    %-----------------------------------------------------------------%

  Real(Kind = wp) Function Box_Muller_gaussian_1(ave, sigma)

    !    %-----------------------------------------------------------------%
    !    | author           - y.afshar           January     2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Real(Kind = wp), Intent(In   ) :: ave, sigma

    Real(Kind = wp), Save :: gauss0
    Real(Kind = wp)       :: gauss1, gauss2

    Logical,         Save :: newjob, job
    Data                     newjob, job /.True., .True./

    If (newjob) Then
       newjob=.False.

       gauss0=uni()

    End If

    ! generate uniform random numbers on (-1, 1)
    Select Case (job)

       Case (.True.)
          job=.False.
          gauss0=1.0_wp
          Do While (gauss0 >= 1.0_wp .Or. gauss0 == 0.0_wp)

             gauss1=2.0_wp*uni()-1.0_wp
             gauss2=2.0_wp*uni()-1.0_wp
             gauss0=gauss1**2+gauss2**2

          End Do

          ! calculate gaussian random numbers
          gauss0=sigma*Sqrt(-2.0_wp*Log(gauss0)/gauss0)
          Box_Muller_gaussian_1=gauss0*gauss1+ave
          gauss0=gauss0*gauss2+ave
       Case Default
          Box_Muller_gaussian_1=gauss0
          job=.True.

    End Select

  End Function Box_Muller_gaussian_1

  !    %-----------------------------------------------------------------%
  !    |  routine using the box-muller method for generating gaussian
  !    |  random numbers with mean "0" and standard deviation "1".
  !    %-----------------------------------------------------------------%

  Real(Kind = wp) Function Box_Muller_gaussian_2()

    !    %-----------------------------------------------------------------%
    !    | author           - y.afshar           January     2010
    !    %-----------------------------------------------------------------%

    Implicit None

    Real(Kind = wp), Save :: gauss0
    Real(Kind = wp)       :: gauss1, gauss2

    Logical,         Save :: newjob, job
    Data                     newjob, job /.True., .True./

    If (newjob) Then
       newjob=.False.

       gauss0=uni()

    End If

    ! generate uniform random numbers on (-1, 1)
    Select Case (job)

       Case (.True.)
          job=.False.
          gauss0=1.0_wp
          Do While (gauss0 >= 1.0_wp .Or. gauss0 == 0.0_wp)

             gauss1=2.0_wp*uni()-1.0_wp
             gauss2=2.0_wp*uni()-1.0_wp
             gauss0=gauss1**2+gauss2**2

          End Do

          ! calculate gaussian random numbers
          gauss0=Sqrt(-2.0_wp*Log(gauss0)/gauss0)
          Box_Muller_gaussian_2=gauss0*gauss1
          gauss0=gauss0*gauss2
       Case Default
          Box_Muller_gaussian_2=gauss0
          job=.True.

    End Select

  End Function Box_Muller_gaussian_2

End Module random_module