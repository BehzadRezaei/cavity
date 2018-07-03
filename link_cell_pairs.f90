Subroutine link_cell_pairs(imcon, rcut)

!    %------------------------------------------------------------------------%
!    |  subroutine for the verlet neighbour list based on link-cell method.   |
!    %------------------------------------------------------------------------%

  Use kinds_f90
  Use setup_module
  Use config_module,  Only : natms,nlast,lstfrz,list,ltg,                      &
                             xxx,yyy,zzz,xxxmax,yyymax,zzzmax,                 &
                                         xxxmin,yyymin,zzzmin

  Implicit None

  Integer,            Intent( In    ) :: imcon
  Real( Kind = wp ) , Intent( In    ) :: rcut

  Logical,                       Save :: newjob
  Data                                   newjob / .true. /

  Logical                             :: safe,lx0,lx1,ly0,ly1,lz0,lz1

  Real( Kind = wp ),             Save :: cutx,cuty,cutz
  Integer,                       Save :: nlx,nly,nlz

  Integer                             :: fail(1:3),icell,ncells,ipass,kk,ll,   &
                                         ibig,i,j, ii,jj,j_start,              &
                                         nlx0s,nly0s,nlz0s, nlx0e,nly0e,nlz0e, &
                                         nlx1s,nly1s,nlz1s, nlx1e,nly1e,nlz1e, &
                                         ix,iy,iz,ic, ix1,ix2,iy1,iy2,iz1,iz2, &
                                         jx,jy,jz,jc,nnatms

  Real( Kind = wp )                   :: rsq,cut,cuti,rcsq,                    &
                                         xij,yij,zij,xmin,ymin,zmin,           &
                                         xspread,yspread,zspread


  Integer, Dimension(:), Allocatable  :: lct,lct_count,lct_start,              &
                                         lct_where,which_cell,at_list

  Integer, Dimension( 1:14 ), Save    :: nix,niy,niz
  Data                                   nix / 0,1,-1,0,1,-1, 0, 1,-1,0,1,-1,0,1 /
  Data                                   niy / 0,0, 1,1,1,-1,-1,-1, 0,0,0, 1,1,1 /
  Data                                   niz / 0,0, 0,0,0, 1, 1, 1, 1,1,1, 1,1,1 /

! At the start of the program

  If (newjob .or. imcon == 0) Then
     newjob=.false.

! Real space cutoff and inverse cutoff, cuti

     cut=rcut+1.0e-6_wp
     cuti=1.0_wp/cut

! Calculate the spread of the domain 
     nnatms=natms

     If (imcon == 0) nnatms=nlast

     Call grid(imcon,nnatms)

     xspread=xxxmax-xxxmin
     yspread=yyymax-yyymin
     zspread=zzzmax-zzzmin

! Calculate the number of link-cells per domain in every direction

     nlx=Int(xspread*cuti)
     nly=Int(yspread*cuti)
     nlz=Int(zspread*cuti)

     cutx=(xspread/Real(nlx,wp))
     cuty=(yspread/Real(nly,wp))
     cutz=(zspread/Real(nlz,wp))

     Write(nrite,'(2x,a,/,5x,i9,1x,i9,1x,i9)')                                 &
           'Number of link-cells per domain in every direction (x, y, z):',    &
            nlx, nly, nlz
     Write(nrite,'(2x,a,/,5x,f9.3,1x,f9.3,1x,f9.3)')                           &
           'Distance between faces of short range link-cells:',                &
           cutx, cuty, cutz
     Write(nrite,'(2x,a,f9.3,/,/)')                                            &
           'Resulting cutoff from subcell neighborhoods is: ', cut

  End If

! Squared r.s.c.

  rcsq=rcut**2

! Check for link cell algorithm violations

  If (nlx*nly*nlz == 0) Call error(20)

  ncells=(nlx+2)*(nly+2)*(nlz+2)

  If (ncells > mxcell) Then
     Call warning(30,Real(ncells,wp),Real(mxcell,wp),0.0_wp)
     Call error(30)
  End If

! The number of neighbour cells plus cell itself 
! That must be checked for producing link-list

  fail=0
  Allocate (which_cell(1:mxatms),at_list(1:mxatms),       Stat=fail(1))
  Allocate (lct(1:ncells),lct_count(1:ncells),                                 &
            lct_start(1:ncells+1),lct_where(1:ncells+1),  Stat=fail(1))

  If (Any(fail > 0)) Then
     Write(nrite,'(/,1x,a)') 'link_cell_pairs allocation failure'
     Call error(0)
  End If

! Schematic of neighbour cells plus cell itself
!
!
!      12---------13---------14
!                   \
!                     \
!            9---------10---------11
!                       | \
!                       |   \
!                   6---------7---------8
!                       |
!                       |
!                       |
!        3---------4----|----5            y
!                   \   |                  \   |z
!                     \ |                    \ |
!              ---------1---------2             ----x
!                       | \
!                       |   \
!                    ------------------
!                       |
!                       |
!                       |
!         --------- ----|----
!                   \   |
!                     \ |
!              --------- ---------
!                         \
!                           \
!                    ------------------

!comment
!
!
!   iy
!      ---------------------------------------------------------------
!      |(ncelly-1)* |(ncelly-1)* |(ncelly-1)* | ...... |ncellx*ncelly|
!      | ncellx+1+z | ncellx+2+z | ncellx+3+z |        |    + z      |
!      ---------------------------------------------------------------
!      |                                                             |
!      |                                                             |
!      |                                                             |
!      |                                                             |
!      |                                                             |
!      ---------------------------------------------------------------
!  ^ 1 | ncellx+1+z | ncellx+2+z | ncellx+3+z | ...... |  2ncellx+ z |
!  |   ---------------------------------------------------------------
!  | 0 |   1  + z   |   2 + z    |   3  + z   | ...... |  ncellx + z |
!  |   ---------------------------------------------------------------
!  Y         0            1            2                  ncellx-1         ix
!      x------>
!
! with z=0 for the first layer (starting from cartesian z = 0),
!      z=1 for the second layer
!      ...
!      z=(ncellz-1) for the last layer (maximum cartesian z value)
!
!
!comment_end


! Form linked list
! Initilize link arrays

  Do i=1,ncells
     lct(i)=0
  End Do

! Initilize cell contents counter

  lct_count = 0

! ALL particles in link-cell space:
! (0,0,0) left-most link-cell on the domain (halo)
! (nlx+3,nly+3,nly+3) right-most
! link-cell on the domain (halo)


! LC limits
! halo -start

  nlx0s=0
  nly0s=0
  nlz0s=0

! halo -end

  nlx0e=0
  nly0e=0
  nlz0e=0

! halo +start

  nlx1s=nlx+1
  nly1s=nly+1
  nlz1s=nlz+1

! halo +end

  nlx1e=nlx+1
  nly1e=nly+1
  nlz1e=nlz+1


!                    ----------------------------------
!                  /                          /      / |
!                /                           --- ---   |
!              /                            |    1e |  |
!            /                              |       |  |
!          /                                | 1s    |/ |
!        /                                   --- ---   |
!      /                                               |
!      --------------------------------                |
!     |                                |               |
!     |                                |               |
!     |                                |               |
!     |                                |               |
!     |                                |               |
!     |              ----------        |               |
!     |             |          |       |               |
!     |             |          |       |               |
!     |             |  Domain  |       |               |
!     |             |          |       |               |
!     |             |          |       |               |
!     |             |          |       |               |
!     |             |          |       |               |
!     |             |          |       |               |
!     |             |          |       |               /
!     |              ----------        |              /
!     |                                |            /
!     |  --------                      |          /
!     |/       / |                     |        /
!     |-------   |              -------|      /
!     |    0e |  |             |       |    /
!     |       |  |             |       |  /
!     | 0s    | /              |       |/
!      --------------------------------
!
!

  xmin=xxxmin-1.0_wp*cutx
  ymin=yyymin-1.0_wp*cuty
  zmin=zzzmin-1.0_wp*cutz

  Do i=1,nlast

! Put all particles in bounded link-cell space: lower and upper bound

     ix = Int((xxx(i)-xmin)/cutx)
     iy = Int((yyy(i)-ymin)/cuty)
     iz = Int((zzz(i)-zmin)/cutz)


! Correction for particles (1,natms) belonging to this domain
! but due to some tiny numerical inaccuracy kicked into
! the halo link-cell space and vice-versa particles from the
! halo (natms+1,nlast) kicked into the domain link-cell space

     If (imcon == 0) Goto 30

     If (i <= natms) Then
        If (ix <= nlx0e) ix=nlx0e+1
        If (ix >= nlx1s) ix=nlx1s-1

        If (iy <= nly0e) iy=nly0e+1
        If (iy >= nly1s) iy=nly1s-1

        If (iz <= nlz0e) iz=nlz0e+1
        If (iz >= nlz1s) iz=nlz1s-1

     Else

        If (Mod(ix,nlx1s)*Mod(iy,nly1s)*Mod(iz,nlz1s) /= 0) Then

           If (Mod(ix,nlx1s) == nlx .or. Mod(ix,nlx1s) == nlx0e + 1)           &
              ix = Int((xxx(i)-xmin)/cutx+1.0e-6_wp*xxx(i))

           If (Mod(iy,nly1s) == nly .or. Mod(iy,nly1s) == nly0e + 1)           &
              iy = Int((yyy(i)-ymin)/cuty+1.0e-6_wp*yyy(i))

           If (Mod(iz,nlz1s) == nlz .or. Mod(iz,nlz1s) == nlz0e + 1)           &
              iz = Int((zzz(i)-zmin)/cutz+1.0e-6_wp*zzz(i))

        End If

     End If

! Hypercube function transformation (counting starts from one
! rather than zero / and 4 more link-cells per
! dimension are accounted /coming from the halo/)

30   icell =1+ix+(nlx+2)*(iy+(nly+2)*(iz))

! at the end of the do-loop lct will point to the head of chain
! for this link-cell (update of lct(icell))

     lct(icell) = i

! Count cell content

     lct_count(icell) = lct_count(icell) + 1

! Backwards relationship

     which_cell(i) = icell

  End Do

! Break down local list to list of linked-cell lists

  lct_start(1) = 1
  Do i=2,ncells+1
     lct_start(i) = lct_start(i-1) + lct_count(i-1)
  End Do

! Domain local to linked-lists local mapping

  lct_where = lct_start
  Do i=1,nlast
     at_list( lct_where( which_cell( i ) ) ) = i
     lct_where( which_cell( i ) ) = lct_where( which_cell( i ) ) + 1
  End Do

! Initialize verlet neighbourlist arrays

  list(0,:)=0

! Initial values of control variables

  ibig=0
  safe=.true.

! Loop over the domain's link-cells only (ipass=1) and
! over the domain's border link-cells only (ipass=2)

  Do ipass=1, 2

    If (ipass == 2 .and. imcon == 1) Cycle

! Primary loop over domain subcells

    Do iz=nlz0e+1,nlz1s-1
       iz1=iz-nlz0e
       iz2=iz-nlz1s
       Do iy=nly0e+1,nly1s-1
          iy1=iy-nly0e
          iy2=iy-nly1s
          Do ix=nlx0e+1,nlx1s-1
             ix1=ix-nlx0e
             ix2=ix-nlx1s

! When ipass = 2 be on the domain's border link-cells

             If ((ipass ==  1) .or.                  &
                   (ix1 ==  1) .or. (ix2 == -1) .or. &
                   (iy1 ==  1) .or. (iy2 == -1) .or. &
                   (iz1 ==  1) .or. (iz2 == -1)) Then

! Index of primary cell

                ic=1+ix+(nlx+2)*(iy+(nly+2)*iz)

! Bypass cell if empty

                If (lct(ic) > 0) Then

! Secondary loop over subcells

                   Do kk=ipass,14

                      If (ipass == 1) Then
                         jx=ix+nix(kk)
                         jy=iy+niy(kk)
                         jz=iz+niz(kk)
                      Else
                         jx=ix-nix(kk)
                         jy=iy-niy(kk)
                         jz=iz-niz(kk)
                      End If


! When ipass = 2 be on the halo link-cells
                      If ( (ipass == 1) .or.                     &
                           (jx <= nlx0e) .or. (jx >= nlx1s) .or. &
                           (jy <= nly0e) .or. (jy >= nly1s) .or. &
                           (jz <= nlz0e) .or. (jz >= nlz1s) ) Then

! Index of neighbouring cell

                           If (imcon == 1) Then

                              If (jx <= nlx0e) jx=jx+nlx
                              If (jx >= nlx1s) jx=jx-nlx
                              If (jy <= nly0e) jy=jy+nly
                              If (jy >= nly1s) jy=jy-nly
                              If (jz <= nlz0e) jz=jz+nlz
                              If (jz >= nlz1s) jz=jz-nlz

                           End If

                           jc=1+jx+(nlx+2)*(jy+(nly+2)*jz)

! Bypass cell if empty

                           If (lct(jc) > 0) Then

! Loop over primary cell contents

                              Do ii=lct_start(ic),lct_start(ic+1)-1

! Get domain local particle index
! Index of cell particles

                                 i=at_list(ii)

! bypass if primary cell particle > natms

                                 If (i <= natms) Then

! Get the secondary list particle index in linked-list local description

                                    If (jc /= ic) Then

! If j-th subcell is not the i-th subcell
! get head of chain of j-th subcell

                                       j_start=lct_start(jc)

                                    Else

! If j-th subcell is the same as the i-th subcell
! get next in line in the linked list

                                       j_start=ii+1

                                    End If

! Loop over secondary cell contents

                                    Do jj = j_start,lct_start(jc+1)-1

! Get domain local particle index
! index of neighbour cell particles (some how cell itself)

                                       j=at_list(jj)

                                       If (ipass == 2 .and. ltg(j) <= natms) Cycle

! Test for frozen atom pairs (frozen atom pairs DO NOT interact)
! If both atoms are for example boundary kind atoms, they DO NOT interact
! If at least one of them is not frozen atom they DO interact

                                       If (lstfrz(i)*lstfrz(j) == 0) Then

                                          xij=xxx(j)-xxx(i)
                                          yij=yyy(j)-yyy(i)
                                          zij=zzz(j)-zzz(i)

                                          If (imcon == 1) Call images(imcon,xij,yij,zij)

                                          rsq=xij**2+yij**2+zij**2

! Check cutoff criterion (atom pairs MUST BE within the cutoff)

                                          If (rsq <= rcsq) Then

! Check for overfloat and add an entry

                                             ll=list(0,i)+1
                                             If (ll > mxlist) Then
                                                ibig=Max(ibig,ll)
                                                safe=.false.
                                             Else
                                                ibig=Max(ibig,ll)
                                                list(0,i)=ll
                                                list(ll,i)=j
                                             End If

! End of if-block for cutoff criterion

                                          End If

! End of if-block on non-frozen atoms

                                       End If

! End of loop over secondary cell contents

                                    End Do

! End of bypass if primary cell particle > natms

                                 End If

! End of loop over primary cell contents

                              End Do

! End of bypass of empty subcell jc

                           End If

! End of inner if-block on cells and borders

                      End If

! End of secondary loop over subcells

                   End Do

! End of bypass of empty subcell ic

                End If

! End of outer if-block on cells and borders

             End If

! End of loops over ix,iy,iz

          End Do

       End Do

    End Do

! End of loop over passes

  End Do

 fail=0

! Terminate job if neighbour list array exceeded

  If (.not.safe) Then
     Call warning(40,Real(ibig,wp),Real(mxlist,wp),0.0_wp)
     Call error(40)
  End If

  Deallocate (which_cell,at_list,                Stat=fail(1))
  Deallocate (lct,lct_count,lct_start,lct_where, Stat=fail(2))

  If (Any(fail > 0)) Then
     Write(nrite,'(/, 1x, a)') 'link_cell_pairs deallocation failure'
     Call error(0)
  End If

End Subroutine link_cell_pairs