!!!!!!!!!!!!!!!!!!!!!!!! THIS IS NUMERIC_ROUTINES !!!!!!!!!!!!!!!!!!!!!
!
! Subroutine grid - give the spread of domain including
!                   particles and boundary particles plus minimum
!                   position in the whole domain
!
! Subroutine dcell - calculates the dimensional properies of a
!                    simulation cell
!
! Subroutine images - calculates the minimum image distance of
!                     atom pairs within a specified cell
!
! Subroutine pbcshift - calculates the minimum image of atoms within a 
!                       specified cell in accordance with boundary condition
!
! Subroutine gtime -
!
! Function wtime -
!
! Subroutine getfn -
!
! Subroutine bcshift-
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Subroutine dcell(aaa,bbb)

!    %-------------------------------------------------------------------------------%
!    |  subroutine to calculate the dimensional properies of a                       |
!    |  simulation cell specified by the input 3x3 matrix aaa (cell vectors in       |
!    |  rows, the matrix is in the form of one dimensional reading (row1,row2,row3). |
!    |                                                                               |
!    |  The results are returned in the array bbb, with:                             |
!    |                                                                               |
!    |  bbb(1 to 3) - lengths of cell vectors                                        |
!    |  bbb(4 to 6) - cosines of cell angles                                         |
!    |  bbb(7 to 9) - perpendicular cell widths                                      |
!    |  bbb(10)     - cell volume                                                    |
!    %-------------------------------------------------------------------------------%

  Use kinds_f90

  Implicit None

  Real( Kind = wp ), Dimension( 1:9 ),  Intent( In    ) :: aaa
  Real( Kind = wp ), Dimension( 1:10 ), Intent(   Out ) :: bbb

  Real( Kind = wp ) :: axb1,axb2,axb3,bxc1,bxc2,bxc3,cxa1,cxa2,cxa3

! calculate lengths of cell vectors

  bbb(1)=Sqrt(aaa(1)*aaa(1)+aaa(2)*aaa(2)+aaa(3)*aaa(3))
  bbb(2)=Sqrt(aaa(4)*aaa(4)+aaa(5)*aaa(5)+aaa(6)*aaa(6))
  bbb(3)=Sqrt(aaa(7)*aaa(7)+aaa(8)*aaa(8)+aaa(9)*aaa(9))

! calculate cosines of cell angles

  bbb(4)=(aaa(1)*aaa(4)+aaa(2)*aaa(5)+aaa(3)*aaa(6))/(bbb(1)*bbb(2))
  bbb(5)=(aaa(1)*aaa(7)+aaa(2)*aaa(8)+aaa(3)*aaa(9))/(bbb(1)*bbb(3))
  bbb(6)=(aaa(4)*aaa(7)+aaa(5)*aaa(8)+aaa(6)*aaa(9))/(bbb(2)*bbb(3))

! calculate vector products of cell vectors

  axb1=aaa(2)*aaa(6)-aaa(3)*aaa(5)
  axb2=aaa(3)*aaa(4)-aaa(1)*aaa(6)
  axb3=aaa(1)*aaa(5)-aaa(2)*aaa(4)

  bxc1=aaa(5)*aaa(9)-aaa(6)*aaa(8)
  bxc2=aaa(6)*aaa(7)-aaa(4)*aaa(9)
  bxc3=aaa(4)*aaa(8)-aaa(5)*aaa(7)

  cxa1=aaa(8)*aaa(3)-aaa(2)*aaa(9)
  cxa2=aaa(1)*aaa(9)-aaa(3)*aaa(7)
  cxa3=aaa(2)*aaa(7)-aaa(1)*aaa(8)

! calculate volume of cell

  bbb(10)=Abs(aaa(1)*bxc1+aaa(2)*bxc2+aaa(3)*bxc3)

! calculate cell perpendicular widths

  bbb(7)=bbb(10)/Sqrt(bxc1*bxc1+bxc2*bxc2+bxc3*bxc3)
  bbb(8)=bbb(10)/Sqrt(cxa1*cxa1+cxa2*cxa2+cxa3*cxa3)
  bbb(9)=bbb(10)/Sqrt(axb1*axb1+axb2*axb2+axb3*axb3)

End Subroutine dcell

!***********************************************************************

Subroutine grid(imcon,n)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  Use kinds_f90
  Use config_module,      Only : celx,cely,celz,xxx,yyy,zzz,                   &
                                 xxxmax,yyymax,zzzmax,                         &
                                 xxxmin,yyymin,zzzmin


  Implicit None

  Integer, Intent( In    ) :: n
  Integer, Intent( In    ) :: imcon

  Integer                  :: i

  If (imcon == 0) Then

     xxxmax=xxx(1)
     yyymax=yyy(1)
     zzzmax=zzz(1)
     xxxmin=xxx(1)
     yyymin=yyy(1)
     zzzmin=zzz(1)

     Do i=2,n

        xxxmax=Max(xxxmax,xxx(i))
        yyymax=Max(yyymax,yyy(i))
        zzzmax=Max(zzzmax,zzz(i))
        xxxmin=Min(xxxmin,xxx(i))
        yyymin=Min(yyymin,yyy(i))
        zzzmin=Min(zzzmin,zzz(i))

     End Do

! rectangular periodic boundary conditions

  Else if( imcon /= 0 ) Then

     xxxmax=celx/2.0_wp
     yyymax=cely/2.0_wp
     zzzmax=celz/2.0_wp
     xxxmin=-xxxmax
     yyymin=-yyymax
     zzzmin=-zzzmax

  End If

End Subroutine grid

!***********************************************************************

Subroutine images(imcon,xxx,yyy,zzz)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! subroutine for calculating the minimum image distance of
! particle pairs within a specified cell.
! The cell matrix is in the form of one dimensional array reading (row1,row2,row3)
!
! Image conditions
!
! imcon=0 applying boundary conditions that do not need to care in a special way
! imcon=1 orthorhombic boundaries apply
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  Use kinds_f90
  Use config_module,    Only : celx,cely,celz

  Implicit None

  Integer,           Intent( In    ) :: imcon
  Real( Kind = wp ), Intent( InOut ) :: xxx,yyy,zzz
  Real( Kind = wp )                  :: aaa,bbb,ccc


  If (imcon == 0) Then

     Return

  Else If (imcon == 1) Then

     aaa=1.0_wp/celx
     bbb=1.0_wp/cely
     ccc=1.0_wp/celz

     xxx=xxx-celx*Anint(aaa*xxx)
     yyy=yyy-cely*Anint(bbb*yyy)
     zzz=zzz-celz*Anint(ccc*zzz)

  Else If (imcon == 2) Then

     aaa=1.0_wp/celx
     bbb=1.0_wp/cely

     xxx=xxx-celx*Anint(aaa*xxx)
     yyy=yyy-cely*Anint(bbb*yyy)

  Else If (imcon == 3) Then

     bbb=1.0_wp/cely

     yyy=yyy-cely*Anint(bbb*yyy)

  Else If (imcon == 4) Then

     Return

  End If

End Subroutine images

!***********************************************************************

Subroutine pbcshift(imcon,time)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! subroutine for calculating the minimum image of atoms within
! a specified cell
!
! imcon=0 applying boundary conditions that not need to care in a special way
! imcon=1 standard cubic boundaries apply
! Note: in this case the centre of the DPD cell is at (0,0,0)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  Use kinds_f90
  Use setup_module,     Only : half_minus
  Use config_module,    Only : natms,celx,cely,celz,                           &
                               xxx,yyy,zzz,vxx
  Use param_module,     Only : shrate

  Implicit None

  Integer,           Intent( In    ) :: imcon
  Real( Kind = wp ), Intent( In    ) :: time 

  Integer                  :: i
  Real( Kind = wp )        :: aaa,bbb,ccc,xss,yss,zss,vwall

  half_minus = Nearest(0.5_wp,-1.0_wp)

  If (imcon == 0) Then

     Return

  Else If (imcon == 1) Then

     aaa=1.0_wp/celx
     bbb=1.0_wp/cely
     ccc=1.0_wp/celz

     Do i=1,natms

        xss=aaa*xxx(i)
        yss=bbb*yyy(i)
        zss=ccc*zzz(i)

        xss=xss-Anint(xss) ; If (xss >= half_minus) xss=-xss
        yss=yss-Anint(yss) ; If (yss >= half_minus) yss=-yss
        zss=zss-Anint(zss) ; If (zss >= half_minus) zss=-zss

        xxx(i)=celx*xss
        yyy(i)=cely*yss
        zzz(i)=celz*zss

     End Do
  
  Else If (imcon == 2) Then 
  
     aaa=1.0_wp/celx
     bbb=1.0_wp/cely

     Do i=1,natms

        xss=aaa*xxx(i)
        yss=bbb*yyy(i)

        xss=xss-Anint(xss) ; If (xss >= half_minus) xss=-xss
        yss=yss-Anint(yss) ; If (yss >= half_minus) yss=-yss

        xxx(i)=celx*xss
        yyy(i)=cely*yss

     End Do

  Else If (imcon == 3) Then 

     bbb=1.0_wp/cely

     Do i=1,natms

        yss=bbb*yyy(i)

        yss=yss-Anint(yss) ; If (yss >= half_minus) yss=-yss

        yyy(i)=cely*yss

     End Do

  Else If (imcon == 4) Then

     vwall=shrate*celz

     aaa=1.0_wp/celx
     bbb=1.0_wp/cely
     ccc=1.0_wp/celz

     Do i=1,natms

        xss=aaa*xxx(i)
        yss=bbb*yyy(i)
        zss=ccc*zzz(i)

        xss   =xss-Mod(vwall*time,celx)*aaa*Anint(zss)

        vxx(i)=vxx(i)-vwall*Anint(zss)
        If (Abs(zss) > 0.5_wp) vxx(i)=-vwall*Anint(zss)/2.0_wp

        xss=xss-Anint(xss) ; If (xss >= half_minus) xss=-xss
        yss=yss-Anint(yss) ; If (yss >= half_minus) yss=-yss
        zss=zss-Anint(zss) ; If (zss >= half_minus) zss=-zss

        xxx(i)=celx*xss
        yyy(i)=cely*yss
        zzz(i)=celz*zss

     End Do

  End If

End Subroutine pbcshift

!***********************************************************************

Subroutine gtime(time)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! timing routine
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  Use kinds_f90

  Implicit None

  Real( Kind = wp ), Intent(   Out ) :: time

  Logical,           Save :: newjob
  Data                       newjob /.true./
  Real( Kind = wp ), Save :: tzero
  REal( Kind = wp )       :: wtime

  If (newjob) Then

     newjob = .false.

     tzero = wtime()
     time = 0.0_wp

  Else

     time = wtime()-tzero

  End If

End Subroutine gtime

!***********************************************************************

Function wtime()

  Use kinds_f90

  Implicit None

  Real( Kind = wp ) :: wtime

  Logical,     Save :: newjob
  Data                 newjob / .true. /
  Character,   Save :: date*8
  Data                 date / ' ' /
  Integer,     Save :: days
  Data                 days / 0 /

  Character         :: date1*8,time*10,zone*5
  Integer           :: value(1:8)

  If (newjob) Then
     newjob = .false.

     Call date_and_time(date,time,zone,value)

     wtime = Real(value(5),wp)*3600.0_wp + Real(value(6),wp)*60.0_wp   +       &
             Real(value(7),wp)           + Real(value(8),wp)/1000.0_wp
  Else

     Call date_and_time(date1,time,zone,value)

! time-per-timestep & start-up and close-down times
! are assumed to be shorter than 24h

     If (date /= date1) Then
        date = date1
        days = days + 1
     End If

     wtime = Real(value(5),wp)*3600.0_wp + Real(value(6),wp)*60.0_wp   +       &
             Real(value(7),wp)           + Real(value(8),wp)/1000.0_wp +       &
             Real(days,wp)*86400.0_wp

  End If

End Function wtime

!***********************************************************************

Subroutine getfn(fname,n,lm,outfile)

  Implicit None

  Integer,               Intent( In    ) :: n,lm
  Character( Len = * ),  Intent( In    ) :: fname
  Character( Len = * ),  Intent(   Out ) :: outfile

  Integer                                :: m1,m2,m3,m4,m5,m6,k,n1

  n1=Int(n/lm)
  k = Len(fname)
  outfile(1:k)=fname

  m1=mod(n1,       10)
  m2=mod(n1/10,    10)
  m3=mod(n1/100,   10)
  m4=mod(n1/1000,  10)
  m5=mod(n1/10000, 10)
  m6=mod(n1/100000,10)

  Write(outfile(k+1:k+6),'(6i1)')m6,m5,m4,m3,m2,m1

End Subroutine getfn

!***********************************************************************

Subroutine bcshift(imcon,mdir,tstep,time)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! subroutine for imposing the boundary condition 
!
! imcon=0 applying boundary conditions that not need to care in a special way
! imcon=1 standard cubic boundaries apply
! imcon=2 Slab (x,y periodic, z non-periodic)
! imcon=3 2D (y periodic, x,z non-periodic)
!
! Note: in this case the centre of the DPD cell is at (0,0,0)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  Use kinds_f90
  Use setup_module,     Only : half_plus,half_minus
  Use config_module,    Only : natms,nlast,lstfrz,                             &
                               celx,cely,celz,                                 &
                               xxx,yyy,zzz,vxx,vyy,vzz

  Use param_module,     Only : shrate  

  Implicit None

  Integer,           Intent( In    ) :: imcon,mdir
  Real( Kind = wp ), Intent( In    ) :: tstep,time

  Integer                  :: i
  Real( Kind = wp )        :: aaa,bbb,ccc,xss,yss,zss,vwall

  half_plus  = Nearest(0.5_wp,1.0_wp)
  half_minus = Nearest(0.5_wp,-1.0_wp)

  If (.not.(imcon == 3 .or. imcon == 4)) Then

     Return

  Else If (imcon == 3) Then 
  
!!!!!     aaa=1.0_wp/celx
!!!!!     ccc=1.0_wp/celz
!!!!!
!!!!!     Do i=1,natms
!!!!!
!!!!!		xss=aaa*xxx(i)
!!!!!		zss=ccc*zzz(i)
!!!!!
!!!!!		If (xss >= half_plus .or. xss <= -half_plus) Then
!!!!!
!!!!!		   vxx(i) = -vxx(i)
!!!!!		   vyy(i) = -vyy(i)
!!!!!		   vzz(i) = -vzz(i)
!!!!!
!!!!!		   Goto 1
!!!!!
!!!!!		Else If (zss >= half_plus .or. zss <= -half_plus) Then
!!!!!
!!!!!		   vxx(i) = -vxx(i)
!!!!!		   vyy(i) = -vyy(i)
!!!!!		   vzz(i) = -vzz(i)
!!!!!
!!!!!		   Goto 1
!!!!!
!!!!!		End If
!!!!!		
!!!!!		Goto 2
!!!!!
!!!!!1		xxx(i)=xxx(i)+tstep*vxx(i)
!!!!!		yyy(i)=yyy(i)+tstep*vyy(i)
!!!!!		zzz(i)=zzz(i)+tstep*vzz(i)
!!!!!
!!!!!2       Continue
!!!!!
!!!!!     End Do 


  Else If (imcon == 4) Then

     vwall=shrate*celz

     aaa=1.0_wp/celx
     ccc=1.0_wp/celz

     Do i=natms+1,nlast

        xss=aaa*xxx(i)
        zss=ccc*zzz(i)

        If      (mdir == -3 .and. lstfrz(i) == 0) Then

           If (zss >= 0.5_wp ) Then
              xss=xss+Mod(vwall*time,celx)*aaa

!               vxx(i)=vxx(i)+1.0_wp*vwall
              vxx(i)=0.5_wp*vwall

              xss=xss-Anint(xss) ; If (xss >= half_minus) xss=-xss

           End If


        Else If (mdir ==  3 .and. lstfrz(i) == 0) Then

           If (zss <= -0.5_wp ) Then
              xss=xss-Mod(vwall*time,celx)*aaa

!               vxx(i)=vxx(i)-1.0_wp*vwall
              vxx(i)=-0.5_wp*vwall


              xss=xss-Anint(xss) ; If (xss >= half_minus) xss=-xss

          End If

       End If

       xxx(i)=celx*xss
       zzz(i)=celz*zss

     End Do

  End If

End Subroutine bcshift


!*******************************


Subroutine bounce_back(imcon,tstep)


  Use kinds_f90
  Use setup_module,     Only : half_plus,half_minus
  Use config_module,    Only : natms,nlast,lstfrz,                             &
                               celx,cely,celz,                                 &
                               xxx,yyy,zzz,vxx,vyy,vzz



  Implicit None

  Integer,           Intent( In    ) :: imcon
  Real( Kind = wp ), Intent( In    ) :: tstep

  Integer                  :: i
  Real( Kind = wp )        :: aaa,ccc,xss,yss,zss

  half_plus  = Nearest(0.5_wp,1.0_wp)
  half_minus = Nearest(0.5_wp,-1.0_wp)

  
     aaa=1.0_wp/celx
     ccc=1.0_wp/celz

     Do i=1,natms

		xss=aaa*xxx(i)
		zss=ccc*zzz(i)

		If (xss >= half_plus .or. xss <= -half_plus) Then

		   vxx(i) = -vxx(i)
		   vyy(i) = -vyy(i)
		   vzz(i) = -vzz(i)

		   Goto 1

		Else If (zss >= half_plus ) Then

		   vxx(i) =1.034_wp-vxx(i)
		   vyy(i) = -vyy(i)
		   vzz(i) = -vzz(i)

		   Goto 1

		Else if (zss <= -half_plus) Then

	 	   vxx(i) = -vxx(i)
		   vyy(i) = -vyy(i)
		   vzz(i) = -vzz(i)

		   Goto 1

		End If
		
		Goto 2

1		xxx(i)=xxx(i)+tstep*vxx(i)
		yyy(i)=yyy(i)+tstep*vyy(i)
		zzz(i)=zzz(i)+tstep*vzz(i)

2       Continue

     End Do 
   


End Subroutine bounce_back