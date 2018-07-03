!    %-----------------------------------------------------------------%
!    |  D P D  --  version 1.4    /    December  2008  --   y.afshar   |
!    %-----------------------------------------------------------------%

!    %------------------------ THIS IS DPD_FORCES ------------------------%
!    |                                                                    |
!    |  Subroutine  force - for computing DPD forces                      |
!    |                      including /Dissipative, Random, Conservative/ |
!    %--------------------------------------------------------------------%

Subroutine force(nstep,engcpe,vircpe,stress,nlast0)

  Use kinds_f90
  Use setup_module
  Use config_module,     Only : natms,nlast,list,xxx,yyy,zzz,                  &
                                vxx,vyy,vzz,fxx,fyy,fzz,ltype,ltg
  Use param_module
  Use statistics_module, Only : numrdf
  Use random_module,     Only : gauss

  Implicit None


  Integer,                             Intent( In    ) :: nstep,nlast0
  Real( Kind = wp ), Dimension( 1:9 ), Intent( InOut ) :: stress
  Real( Kind = wp ),                   Intent(   Out ) :: engcpe,vircpe


  Integer,           Dimension( : ), Allocatable :: ilist
  Real( Kind = wp ), Dimension( : ), Allocatable :: rsq

  Integer            :: fail,i,j,k,limit,kk

  Real( Kind = wp )  :: xij,yij,zij,rrr,eijx,eijy,eijz,vij,  &
                        cons,diss,randf,fkt,vrl,pot,         &
                        fix,fiy,fiz,fx,fy,fz,                &
                        strs1,strs2,strs3,strs4,strs5,strs6, &
                        strs7,strs8,strs9

  fail=0

  Allocate (ilist(0:mxlist),rsq(1:mxlist),       Stat=fail)

  If (fail> 0) Then
     Write(nrite,'(/,1x,a)') 'forces allocation failure'
     Call error(0)
  End If

! initialize potential energy and virial

  pot = 0.0_wp
  vrl = 0.0_wp

  engcpe = 0.0_wp
  vircpe = 0.0_wp

! initialize stress tensor accumulators

  strs1 = 0.0_wp
  strs2 = 0.0_wp
  strs3 = 0.0_wp
  strs4 = 0.0_wp
  strs5 = 0.0_wp
  strs6 = 0.0_wp
  strs7 = 0.0_wp
  strs8 = 0.0_wp
  strs9 = 0.0_wp

  ilist = 0
  rsq   = 0.0_wp

! Set up non-bonded interaction (verlet) list using link cells

  Call link_cell_pairs(imcon,rcut)

  Do i=1,natms

! Get list limit

     limit=list(0,i)
     ilist(0)=limit

! load forces

     fix=fxx(i)
     fiy=fyy(i)
     fiz=fzz(i)

! calculate interparticle distances

     Do k=1,limit

        j=list(k,i)
        ilist(k)=j

        xij=xxx(i)-xxx(j)
        yij=yyy(i)-yyy(j)
        zij=zzz(i)-zzz(j)

! periodic boundary conditions

        Call images(imcon,xij,yij,zij)

! square of distances & distances

        rsq(k)=xij*xij+yij*yij+zij*zij


! If two particles stand on each others

        If (rsq(k).Gt.1.0e-16_wp) Then

           rrr=Sqrt(rsq(k))

! calculate unit vector separating the particles

           eijx=xij/rrr
           eijy=yij/rrr
           eijz=zij/rrr

           vij=(vxx(i)-vxx(j))*eijx+(vyy(i)-vyy(j))*eijy+(vzz(i)-vzz(j))*eijz

           kk=Max(ltype(i),ltype(j))*(Max(ltype(i),ltype(j))-1)/2+Min(ltype(i),ltype(j))

           aij=vvv(1,kk)
           gamma=vvv(2,kk)
           sigma=(2.0_wp*gamma*kt)**0.5_wp

           fkt=Max((1.0_wp-rrr/rvdw)**0.25,0.0_wp)

           pot=pot+0.5_wp*aij*rvdw*fkt*fkt

! calculate conservative force
           cons=aij*Max((1.0_wp-rrr/rvdw),0.0_wp)

! calculate dissipative force
           diss=-gamma*fkt*fkt*vij

! calculate random force
           randf=sigma*fkt*gauss()*tstep**(-0.5_wp)

! calculate total force
           diss=cons+diss+randf

! calculate forces
           fx=diss*eijx
           fy=diss*eijy
           fz=diss*eijz

! calculate the virial contribution
           vrl=vrl-cons*rrr
           fix   =fix   +fx
           fiy   =fiy   +fy
           fiz   =fiz   +fz
           fxx(j)=fxx(j)-fx
           fyy(j)=fyy(j)-fy
           fzz(j)=fzz(j)-fz

! calculate stress tensor

           strs1 = strs1 - xij*fx
           strs2 = strs2 - xij*fy
           strs3 = strs3 - xij*fz
           strs4 = strs4 - yij*fx
           strs5 = strs5 - yij*fy
           strs6 = strs6 - yij*fz
           strs7 = strs7 - zij*fx
           strs8 = strs8 - zij*fy
           strs9 = strs9 - zij*fz

        End If

    End Do

! load back forces

    fxx(i)=fix
    fyy(i)=fiy
    fzz(i)=fiz

! accumulate radial distribution functions

    If ( lrdf .and. (nstep >= nsteql)                                          &
              .and. (nstep > 0 .and. Mod(nstep,nstrdf) == 0) )                 &
       Call rdf_collect(i,rcut,ilist,rsq)

  End Do

! counter for rdf statistics outside loop structure

  If ( lrdf .and. (nstep >= nsteql)                                            &
            .and. (nstep > 0 .and. Mod(nstep,nstrdf) == 0) )                   &
     numrdf = numrdf + 1

! sum forces from halo region
  Do i=nlast0+1, nlast
     fxx(ltg(i))=fxx(ltg(i))+fxx(i)
     fyy(ltg(i))=fyy(ltg(i))+fyy(i)
     fzz(ltg(i))=fzz(ltg(i))+fzz(i)
  End Do

! sum up potentials and virials

  engcpe = engcpe + pot
  vircpe = vircpe + vrl

! complete stress tensor

  stress(1) = stress(1) + strs1
  stress(2) = stress(2) + strs2
  stress(3) = stress(3) + strs3
  stress(4) = stress(4) + strs4
  stress(5) = stress(5) + strs5
  stress(6) = stress(6) + strs6
  stress(7) = stress(7) + strs7
  stress(8) = stress(8) + strs8
  stress(9) = stress(9) + strs9

  Deallocate (ilist,rsq,      Stat=fail)

  If (fail > 0) Then
     Write(nrite,'(/,1x,a)') 'forces deallocation failure'
     Call error(0)
  End If

End Subroutine force
