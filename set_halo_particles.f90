Subroutine set_halo_particles(imcon,rcut,time)

!    %--------------------------------------------------------%
!    | Subroutine to arrange exchange of particles to halo    |
!    %--------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : natms,nlast,ltg,      &
                            celx,cely,celz,       &
                            xxxmax,yyymax,zzzmax, &
                            xxxmin,yyymin,zzzmin

  Implicit None

  Integer,           Intent( In    ) :: imcon
  Real( Kind = wp ), Intent( In    ) :: rcut,time


  Logical,           Save :: newjob
  Data                       newjob / .true. /

  Real( Kind = wp ), Save :: cutx,cuty,cutz
  Real( Kind = wp )       :: xspread,yspread,zspread
  Data                       xspread,yspread,zspread  / 3*0.0_wp /


  Real( Kind = wp )       :: cut,cuti
  Integer                 :: nlx,nly,nlz,i

  If (.not.(imcon == 2 .or. imcon == 3  .or. imcon == 4)) Return

  If (newjob) Then
     newjob=.false.

! Define cut

     cut=rcut+1.0e-6_wp
     cuti=1.0_wp/cut

! Calculate the spread of the domain 

     Call grid(imcon,natms)

     xspread=xxxmax-xxxmin
     yspread=yyymax-yyymin
     zspread=zzzmax-zzzmin

! Calculate the number of link-cells per domain in every direction

     nlx=Int(xspread*cuti)
     nly=Int(yspread*cuti)
     nlz=Int(zspread*cuti)

! Calculate a link-cell width in every direction

     cutx=xspread/Real(nlx,wp)
     cuty=yspread/Real(nly,wp)
     cutz=zspread/Real(nlz,wp)

  End If

  ltg = 0

  Do i=1,nlast
     ltg(i)=i
  End Do

  If      (imcon == 1) Then

! exchange atom data in -/+ x directions
          Call export_data(-1,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 1,celx,cely,celz,cutx,cuty,cutz)

! exchange atom data in -/+ y directions
          Call export_data(-2,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 2,celx,cely,celz,cutx,cuty,cutz)

! exchange atom data in -/+ z directions
          Call export_data(-3,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 3,celx,cely,celz,cutx,cuty,cutz)

  Else If (imcon == 2) Then

! exchange atom data in -/+ x directions
          Call export_data(-1,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 1,celx,cely,celz,cutx,cuty,cutz)

! exchange atom data in -/+ y directions
          Call export_data(-2,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 2,celx,cely,celz,cutx,cuty,cutz)

  Else If (imcon == 3) Then

! exchange atom data in -/+ y directions
          Call export_data(-2,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 2,celx,cely,celz,cutx,cuty,cutz)

  Else If (imcon == 4) Then

! exchange atom data in -/+ z directions
          Call export_data(-3,celx,cely,celz,cutx,cuty,cutz)
          Call bcshift(4,-3,0.,time)
          Call export_data( 3,celx,cely,celz,cutx,cuty,cutz)
          Call bcshift(4, 3,0.,time)

! exchange atom data in -/+ x directions
          Call export_data(-1,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 1,celx,cely,celz,cutx,cuty,cutz)

! exchange atom data in -/+ y directions
          Call export_data(-2,celx,cely,celz,cutx,cuty,cutz)
          Call export_data( 2,celx,cely,celz,cutx,cuty,cutz)

  End If

End Subroutine set_halo_particles
