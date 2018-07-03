Subroutine export_data(mdir,sidex,sidey,sidez,cwx,cwy,cwz)

!    %-----------------------------------------------------------------%
!    |  Subroutine to export atomic data in domain boundary regions    |
!    |  for halo formation                                             |
!    %-----------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : nlast,xxx,yyy,zzz,vxx,vyy,vzz, &
                            ltype,lstfrz,weight,ltg

  Implicit None

  Integer,           Intent( In    ) :: mdir
  Real( Kind = wp ), Intent( In    ) :: sidex,sidey,sidez,cwx,cwy,cwz

  Logical           :: safe
  Integer           :: i
  Real( Kind = wp ) :: shovex,shovey,shovez,begin,final,xyz


! DIRECTION SETTINGS INITIALISATION

! define the relative displacement
! in order to push all particles of this domain in the direction of mdir

! define 'minus halo' limits in the direction of mdir (begin,final)
! |begin-final| is the link cell width (cwx,cwy,cwz) <--> (cutx,cuty,cutz)

! Direction -x

  If      (mdir == -1) Then

     shovex=sidex
     shovey=0.0_wp
     shovez=0.0_wp

     begin=-0.5_wp*sidex
     final=begin+cwx


! Direction +x

  Else If (mdir ==  1) Then

     shovex=-sidex
     shovey=0.0_wp
     shovez=0.0_wp

     final=0.5_wp*sidex
     begin=final-cwx


! Direction -y

  Else If (mdir == -2) Then

     shovex=0.0_wp
     shovey=sidey
     shovez=0.0_wp

     begin=-0.5_wp*sidey
     final=begin+cwy


! Direction +y

  Else If (mdir ==  2) Then

     shovex=0.0_wp
     shovey=-sidey
     shovez=0.0_wp

     final=0.5_wp*sidey
     begin=final-cwy


! Direction -z

  Else If (mdir == -3) Then

     shovex=0.0_wp
     shovey=0.0_wp
     shovez=sidez

     begin=-0.5_wp*sidez
     final=begin+cwz


! Direction +z

  Else If (mdir ==  3) Then

     shovex=0.0_wp
     shovey=0.0_wp
     shovez=-sidez

     final=0.5_wp*sidez
     begin=final-cwz


  Else

     Call error(46)

  End If

! Initialise array overflow flags

  safe=.true.

! LOOP OVER ALL PARTICLES

  Do i=1,nlast

     If      (mdir == -1 .or. mdir == 1) Then

             xyz=xxx(i)

     Else If (mdir == -2 .or. mdir == 2) Then

             xyz=yyy(i)

     Else If (mdir == -3 .or. mdir == 3) Then

             xyz=zzz(i)

     End If

     If (xyz >= begin .and. xyz < final) Then

        If (lstfrz(i) /= 0 .and. xyz == -Abs(begin)) Cycle

        nlast=nlast+1

! Is it safe to proceed?
! Check for array bound overflow (Can arrays cope with incoming data?)

        safe=(nlast <= mxatms)

        If (.not.safe) Then
           Call warning(160,Real(nlast,wp),Real(mxatms,wp),0.0_wp)
           Call error(56)
        End If

        xxx(nlast)=xxx(i)+shovex
        yyy(nlast)=yyy(i)+shovey
        zzz(nlast)=zzz(i)+shovez

! assing incoming atom properties

         vxx(nlast)=vxx(i)
         vyy(nlast)=vyy(i)
         vzz(nlast)=vzz(i)

         ltype(nlast)=ltype(i)
         lstfrz(nlast)=lstfrz(i)
         weight(nlast)=weight(i)
         ltg(nlast)=ltg(i)

     End If

  End Do

End Subroutine export_data
