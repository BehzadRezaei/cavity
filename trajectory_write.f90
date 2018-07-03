Subroutine trajectory_write                                                    &
           (imcon,keyres,ndump,nstraj,istraj,nstep,tstep,time)

!    %------------------------------------------------------------------------------%
!    |  subroutine for writing history file at selected intervals in simulation     |
!    %------------------------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : cell,natms,weight,xxx,yyy,zzz,                     &
                            vxx,vyy,vzz,fxx,fyy,fzz,ltype

  Use statistics_module, Only : rsd

  Implicit None

  Integer,               Intent( In    ) :: imcon,keyres,ndump,                &
                                            nstraj,istraj,nstep

  Real( Kind = wp ),     Intent( In    ) :: tstep,time

  Logical,                          Save :: newopen,l_first
  Data                                      newopen,l_first / .true. , .true. /

  Integer( Kind=ip ),               Save :: rec
  Data                                      rec / 0_ip /

  Character( Len = 7 )        :: fname
  Character( Len = 13 ), Save :: name

  Character     :: line*60,lf*1
  Data             line / '                                                            ' /

  Integer       :: i,index,lm
  Integer, Save :: ln


  If (.not.(nstep >= nstraj .and. Mod(nstep-nstraj,istraj) == 0)) Return

! define newline character Char(10) or Char(Iachar('\n'))

  lf = Char(10)

! interval for opening a new history file
  lm = 100000

  fname='HISTORY'

  ! Be careful every ndump step it will start writing on the previous created HISTORY files
  ! If you do not like it you can change it by multiplying a big number in ndump or change the
  ! ndump with a big number in the below line
!   index=Mod(nstep-nstraj,Max(ndump/istraj,1)*istraj)
  index=Mod(nstep-nstraj, 10000000)

! open the history file if new job or file closed

  If (newopen) Then
     newopen=.false.

     ln=0

     Call getfn(fname,index,(lm*istraj),name)

! Obliterate old HISTORY if not needed

     If (keyres /= 1 .and. l_first) Then

        l_first=.false.
        Open(Unit=nhist, File=name, Form='formatted', Access='direct', Recl=102, Status='replace')
        Close(Unit=nhist)
        rec=Int(0,ip)
        ln=0

     End If

! If the keyres=1 is the file old (does it exist)? Calculate offset pointer

     If (keyres == 1 .and. l_first) Then

        Inquire(File=name, Exist=l_first)

        If (l_first) Then

            l_first=.false.
            Open(Unit=nhist, File=name, Form='formatted')
            Do While (.true.)
               Read(nhist, *, End=1)
               rec=rec+Int(1,ip)
               ln=ln+1
            End Do

1           Continue

            Close(Unit=nhist)

        End If
      End If

  End If

  If (Mod(ln,lm) == 0) Then

      Call getfn(fname,index,(lm*istraj),name)
      Open(Unit=nhist, File=name, Form='formatted', Access='direct', Recl=102, Status='replace')
      rec=Int(0,ip)
      ln=1

  Else

      Open(Unit=nhist, File=name, Form='formatted', Access='direct', Recl=102)
      ln=ln+1

  End If

  !Open(Unit=nhist, File='HISTORY', Form='formatted', Access='direct', Recl=63)

! Regular printing

  rec=rec+Int(1,ip)
  Write(Unit=nhist, Fmt='(i10,2f12.4,a60,a7,a1)', Rec=rec) nstep,tstep,time,line(1:60),line(1:7),lf
  !rec=rec+Int(1,ip)
  !Write(Unit=nhist, Fmt='(2i10,i3,a60,a18,a1)', Rec=rec) natms,nlast,imcon,line(1:60),line(1:18),lf
  !rec=rec+Int(1,ip)
  !Write(Unit=nhist, Fmt='(3f20.10,a2,a1)', Rec=rec) cell(1),cell(2),cell(3),line(1:2),lf
  !rec=rec+Int(1,ip)
  !Write(Unit=nhist, Fmt='(3f20.10,a2,a1)', Rec=rec) cell(4),cell(5),cell(6),line(1:2),lf
  !rec=rec+Int(1,ip)
  !Write(Unit=nhist, Fmt='(3f20.10,a2,a1)', Rec=rec) cell(7),cell(8),cell(9),line(1:2),lf


  !Do i = 1,nlast
  Do i = 1,natms

     rec=rec+Int(1,ip)
     Write(Unit=nhist, Fmt='(f8.4,i2,1p,7e13.5,a1)', Rec=rec) weight(i),       &
          ltype(i),rsd(i),xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i),lf
!      rec=rec+Int(1,ip)
!     Write(Unit=nhist, Fmt='(2f12.6,2i4,a30,a1)', Rec=rec) weight(i),rsd(i),ltype(i),lstfrz(i),line(1:30),lf
!     rec=rec+Int(1,ip)
!     Write(Unit=nhist, Fmt='(1p,3e12.4,a26,a1)', Rec=rec) xxx(i),yyy(i),zzz(i),line(1:26),lf
!     rec=rec+Int(1,ip)
!     Write(Unit=nhist, Fmt='(1p,3e12.4,a26,a1)', Rec=rec) vxx(i),vyy(i),vzz(i),line(1:26),lf
!     rec=rec+Int(1,ip)
!     Write(Unit=nhist, Fmt='(1p,3e12.4,a26,a1)', Rec=rec) fxx(i),fyy(i),fzz(i),line(1:26),lf

  End Do

! close history file at regular intervals

  Close(Unit=nhist)

End Subroutine trajectory_write
