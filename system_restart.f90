Subroutine system_restart                                                       &
           (imcon,rcut,lrdf,nstep,tstep,time,tmst,stress)            

!    %--------------------------------------------------------------------%
!    |  subroutine for writing restart files at job termination or        |
!    |  selected intervals in simulation                                  |
!    %--------------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module
  Use statistics_module
  Use param_module,  Only : rbin,lconf 

  Implicit None

  Integer,           Intent( In    ) :: imcon,nstep
  Logical,           Intent( In    ) :: lrdf
  Real( Kind = wp ), Intent( In    ) :: rcut,tstep,time,tmst,                  &
                                        stress(1:9)
  Character( Len = 9 ) :: name
  Integer              :: i

! Write RESCONFIG

  name = 'RESCONFIG' ! file name

  If (lconf) Call write_config(name,imcon,nstep,tstep,time)

! Write accumulator data to dump file

  Open(nrest, File='RESTART', Form='unformatted', Status='replace')

  Write(nrest) rcut,rbin
  Write(nrest) Real(natms,wp),Real(nlast,wp),Real(nstep,wp),Real(numacc,wp),Real(numrdf,wp),time,tmst
  Write(nrest) stpval
  Write(nrest) stpvl0
  Write(nrest) sumval
  Write(nrest) ssqval
  Write(nrest) zumval
  Write(nrest) ravval
  Write(nrest) stkval
  Write(nrest) stress

  If (lrdf) Write(nrest) rdf

! write initial position and final displacement data to RESTART

  Do i=1,natms

     Write(nrest) xin(i),yin(i),zin(i),xto(i),yto(i),zto(i),                   &
                  xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i),                   &
                  fxx(i),fyy(i),fzz(i),weight(i),                              &
                  Real(lstfrz(i),wp),Real(ltype(i),wp)

  End Do

  Do i=natms+1,nlast

     Write(nrest) xxx(i),yyy(i),zzz(i),vxx(i),vyy(i),vzz(i),                   &
                  fxx(i),fyy(i),fzz(i),weight(i),                              &
                  Real(lstfrz(i),wp),Real(ltype(i),wp)
  End Do

  Close(nrest)

End Subroutine system_restart

!***********************************************************************

Subroutine write_config(name,imcon,nstep,tstep,time)

!    %--------------------------------------------------------------------%
!    |  subroutine for writing configuartion file                         |
!    %--------------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module, Only : cell,natms,nlast,weight,ltype,lstfrz,              &
                            xxx,yyy,zzz,vxx,vyy,vzz,fxx,fyy,fzz
                            

  Implicit None

  Character( Len = * ), Intent( In    ) :: name
  Integer,              Intent( In    ) :: imcon,nstep
  Real( Kind = wp ),    Intent( In    ) :: tstep,time

  Integer(Kind=ip) :: rec
  Character        :: line*60,lf*1
  Data                line / '                                                            ' /

  Integer          :: i


! define newline character Char(10)

  lf = Char(10)

! write configuration data to new configuration file

  Open(Unit=nconf, File=name, Form='formatted', Access='direct', Recl=64)

  Write(Unit=nconf, Fmt='(i3,i10,2f15.8,2i10,a1)', Rec=1) imcon,nstep,tstep,time,natms,nlast,lf

  Write(Unit=nconf, Fmt='(3f20.10,a3,a1)', Rec=2) cell(1),cell(2),cell(3),line(1:3),lf
  Write(Unit=nconf, Fmt='(3f20.10,a3,a1)', Rec=3) cell(4),cell(5),cell(6),line(1:3),lf
  Write(Unit=nconf, Fmt='(3f20.10,a3,a1)', Rec=4) cell(7),cell(8),cell(9),line(1:3),lf


  rec=Int(4,ip)

  Do i=1,nlast
!  Do i=1,natms

     rec=rec+Int(1,ip)
     Write(Unit=nconf, Fmt='(g20.10,2i10,a23,a1)', Rec=rec) weight(i),ltype(i),lstfrz(i),line(1:23),lf 

     rec=rec+Int(1,ip)
     Write(Unit=nconf, Fmt='(3g20.10,a3,a1)', Rec=rec) xxx(i),yyy(i),zzz(i),line(1:3),lf

     rec=rec+Int(1,ip)
     Write(Unit=nconf, Fmt='(3g20.10,a3,a1)', Rec=rec) vxx(i),vyy(i),vzz(i),line(1:3),lf

!     rec=rec+Int(1,ip)
!     Write(Unit=nconf, Fmt='(3g20.10,a3,a1)', Rec=rec) fxx(i),fyy(i),fzz(i),line(1:3),lf

  End Do

  Close(Unit=nconf)

End Subroutine write_config