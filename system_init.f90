Subroutine system_init                                                         &
           (imcon,rcut,lrdf,keyres,time,tmst,nstep,stress)
		   
!    %-----------------------------------------------------------------%
!    |  Subroutine for reading the RESTART file data and defining the  |
!    |  initial data                                                   |
!    %-----------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : volm,natms,nlast,numtyp,                           &
                            dens,ltype,weight,lstfrz,                          &
                            xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz
  Use param_module,  Only : rbin
  Use statistics_module

  Implicit None

  Logical,           Intent( In    ) :: lrdf
  Integer,           Intent( InOut ) :: keyres
  Integer,           Intent( In    ) :: imcon
  Real( Kind = wp ), Intent( In    ) :: rcut

  Integer,           Intent(   Out ) :: nstep
  Real( Kind = wp ), Intent(   Out ) :: time,tmst,stress(1:9)
                                        
  Logical           :: l_tmp
  Integer           :: i,k,keyrdf,keyio,i_tmp
  Real( Kind = wp ) :: dnatms,dnlast,dnstep,dnumac,dnumrd,xyz(1:18)

! Initialize read failure flag

  keyio=0

50 Continue
  If (keyres /=1) Then

! initialize step counters

     numacc=0
     numrdf=0

     nstep =0
     time  =0.0_wp
     tmst  =0.0_wp

! initialize stress

     stress = 0.0_wp

! initialize accumulator arrays if reading failure occured

     If (keyio > 0) Then

        stpval=0.0_wp
        stpvl0=0.0_wp
        sumval=0.0_wp
        ssqval=0.0_wp
        zumval=0.0_wp
        ravval=0.0_wp
        stkval=0.0_wp

        If (lrdf) rdf=0.0_wp

     End If

  End If

! restart simulation and continue

  If (keyres == 1) Then

! If RESTART doesn't exist then abort (mishmashed RESTART is handled separately)

     l_tmp=.true.
     Inquire(File='RESTART', Exist=l_tmp)
     If (.not.l_tmp) Call error(519)

! Check RESTART restart compatibility: rcut,rbin

     xyz(1:2)=0.0_wp
     Open(nrest,file='RESTART',form='unformatted',IOStat=keyio)
     Read(nrest,IOStat=keyio,End=100) xyz(1),xyz(2)

     If (Abs(xyz(1)-rcut) > 1.0e-6_wp .or. Abs(xyz(2)-rbin) > 1.0e-6_wp) Call error(519)

! read the rest of the accumulator data from dump file

     Read(nrest,IOStat=keyio,End=100) dnatms,dnlast,dnstep,dnumac,dnumrd,time,tmst
     Read(nrest,IOStat=keyio,End=100) stpval
     Read(nrest,IOStat=keyio,End=100) stpvl0
     Read(nrest,IOStat=keyio,End=100) sumval
     Read(nrest,IOStat=keyio,End=100) ssqval
     Read(nrest,IOStat=keyio,End=100) zumval
     Read(nrest,IOStat=keyio,End=100) ravval
     Read(nrest,IOStat=keyio,End=100) stkval
     Read(nrest,IOStat=keyio,End=100) stress

     If (lrdf) Read(nrest,IOStat=keyio,End=100) rdf

     natms =Nint(dnatms)
     nlast =Nint(dnlast)
     nstep =Nint(dnstep)
     numacc=Nint(dnumac)
     numrdf=Nint(dnumrd)

100  Continue

! If 'restart' is impossible go to 'restart noscale' and reinitialize

     If (keyio /= 0) Then
        Call warning(190,0.0_wp,0.0_wp,0.0_wp)
        Close(nrest)
        keyres=0
        Go To 50
     End If

  End If

! initialize initial positions to current positions
! and final displacements to zero

  Do i=1,natms
     xin(i)=xxx(i)
     yin(i)=yyy(i)
     zin(i)=zzz(i)

     xto(i)=0.0_wp
     yto(i)=0.0_wp
     zto(i)=0.0_wp
  End Do

  If (keyres == 1) Then

! Error accumulator: keyio is still zero otherwise we cannot get here

     i_tmp=0

     Do i=1,natms

        xyz = 0.0_wp

        Read(nrest,IOStat=keyio) xyz(1),  xyz(2),  xyz(3),                    &
                                 xyz(4),  xyz(5),  xyz(6),                    & 
                                 xyz(7),  xyz(8),  xyz(9),                    &
                                 xyz(10), xyz(11), xyz(12),                   &
                                 xyz(13), xyz(14), xyz(15),                   &
                                 xyz(16), xyz(17), xyz(18)

        If (keyio /= 0) i_tmp=1

! assign particle initial positions and final displacements
! to the corresponding domains

        xin(i)=xyz(1)     ;     xto(i)=xyz(4)
        yin(i)=xyz(2)     ;     yto(i)=xyz(5)
        zin(i)=xyz(3)     ;     zto(i)=xyz(6)


        xxx(i)=xyz(7)     ;     vxx(i)=xyz(10)     ;     fxx(i)=xyz(13)
        yyy(i)=xyz(8)     ;     vyy(i)=xyz(11)     ;     fyy(i)=xyz(14)
        zzz(i)=xyz(9)     ;     vzz(i)=xyz(12)     ;     fzz(i)=xyz(15)	

        weight(i)=xyz(16)
        lstfrz(i)=Nint(xyz(17))
        ltype(i)=Nint(xyz(18))

     End Do

! If 'restart' is impossible go to 'restart noscale' and reinitialize

     If (i_tmp /= 0) Then
        Call warning(190,0.0_wp,0.0_wp,0.0_wp)
        Close(nrest)
        keyres=0
        Go To 50
     End If

     If (nlast /= natms) Then

        Do i=natms+1,nlast

           xyz = 0.0_wp

           Read(nrest,IOStat=keyio) xyz(1),  xyz(2),  xyz(3),                  &
                                    xyz(4),  xyz(5),  xyz(6),                  & 
                                    xyz(7),  xyz(8),  xyz(9),                  &
                                    xyz(10), xyz(11), xyz(12)

           If (keyio /= 0) i_tmp=1

           xxx(i)=xyz(1)     ;     vxx(i)=xyz(4)     ;     fxx(i)=xyz(7)
           yyy(i)=xyz(2)     ;     vyy(i)=xyz(5)     ;     fyy(i)=xyz(8)
           zzz(i)=xyz(3)     ;     vzz(i)=xyz(6)     ;     fzz(i)=xyz(9) 

           weight(i)=xyz(10)
           lstfrz(i)=Nint(xyz(11))
           ltype(i)=Nint(xyz(12))

        End Do

     End If

! If 'restart' is impossible go to 'restart noscale' and reinitialize

     If (i_tmp /= 0) Then
        Call warning(190,0.0_wp,0.0_wp,0.0_wp)
        Close(nrest)
        keyres=0
        Go To 50
     End If

     Close(nrest)

  End If


! number densities needed for long-range corrections

! evaluate species populations in system

  Do i = 1,natms
     k = ltype(i)
     numtyp(k) = numtyp(k)+1.0_wp
  End Do

! number densities

  Do i = 1,mxatyp
     dens(i) = numtyp(i)/volm
  End Do

End Subroutine system_init