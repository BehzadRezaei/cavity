!    %-------------------- THIS IS RDF_ROUTINES -----------------------%
!    |                                                                 |
!    | Subroutine  rdf_collect - subroutine for accumulating statistic |
!    |                           for radial distribution functions     |
!    |                                                                 |
!    | Subroutine  rdf_compute  - subroutine for calculating radial    |
!    |                            distribution functions from          |
!    |                            accumulated data                     |
!    |                                                                 |
!    %-----------------------------------------------------------------%

Subroutine rdf_collect(iatm,rcut,ilist,rsqdf)

!    %-----------------------------------------------------------------%
!    |  subroutine for accumulating statistic for                      |
!    |  radial distribution functions                                  |
!    |                                                                 |
!    |  Note: to be used as part of dpd_forces                         |
!    %-----------------------------------------------------------------%

  Use kinds_f90
  Use setup_module,      Only : mxlist,mxgrdf
  Use config_module,     Only : natms,ltype
  Use statistics_module, Only : lstrdf,rdf,ntprdf
  Use param_module,      Only : keyrdf

  Implicit None

  Integer,                                 Intent( In    ) :: iatm
  Real( Kind = wp ),                       Intent( In    ) :: rcut
  Integer,           Dimension( 0:mxlist), Intent( In    ) :: ilist
  Real( Kind = wp ), Dimension( 1:mxlist), Intent( In    ) :: rsqdf

  Logical,           Save :: newjob
  Data                       newjob / .true. /
  Real( Kind = wp ), Save :: rcsq,rdelr

  Integer                 :: jatm,ai,aj,keyrdf0,kk,ll,m,i
  Real( Kind = wp )       :: rrr,rsq


  If (newjob) Then
     newjob=.false.

! set cutoff condition for pair forces and grid interval for rdf tables

     rcsq = rcut*rcut
     rdelr= Real(mxgrdf,wp)/rcut

     Do i=1,ntprdf

        If (lstrdf(keyrdf(i)) /= 0) Call error(110)

        lstrdf(keyrdf(i))=i

     End Do

  End If


! set up atom iatm type

  ai=ltype(iatm)

! start of primary loop for rdf accumulation

  Do m=1,ilist(0)

! particle indices

     jatm=ilist(m)

     If (jatm <= natms) Then

        aj=ltype(jatm)
        keyrdf0=(Max(ai,aj)*(Max(ai,aj)-1))/2+Min(ai,aj)
        kk=lstrdf(keyrdf0)

! proceed if the rdf pair is specified for look up

        If (kk > 0) Then

! apply truncation of potential

           rsq=rsqdf(m)

           If (rsq < rcsq) Then
              rrr=Sqrt(rsq)
              ll=Int(rrr*rdelr+0.999999_wp)

! accumulate correlation

              rdf(ll,kk) = rdf(ll,kk) + 1.0_wp

           End If

        End If

     End If

  End Do

End Subroutine rdf_collect

!***********************************************************************

Subroutine rdf_compute(rcut)

!    %-----------------------------------------------------------------%
!    |  Subroutine for calculating radial distribution functions       |
!    |  from accumulated data                                          |
!    %-----------------------------------------------------------------%

  Use kinds_f90
  Use setup_module,      Only : mxgrdf,mxatyp,nrite,nrdfdt,pi
  Use config_module,     Only : volm,numtyp,dens
  Use statistics_module, Only : numrdf,ntprdf,lstrdf,rdf


  Implicit None

  Real( Kind = wp ), Intent( In    ) :: rcut

  Logical           :: zero
  Integer           :: i,ia,ib,kk
  Real( Kind = wp ) :: delr,dvol,factor,gofr,gofr1,rrr,sum,sum1
                                                

  

  Write(nrite,"(/,/,1X,'RADIAL DISTRIBUTION FUNCTIONS',/,/,                    &
       ' calculated using ',i10,' configurations')") numrdf

! open RDF file and Write headers

  Open(nrdfdt, File='RDFDAT', Status='replace')
  Write(nrdfdt,'(2i10,/,/)') ntprdf,mxgrdf

! grid interval for rdf tables

  delr=rcut/Real(mxgrdf,wp)

! for all possible unique type-to-type pairs

  Do ia=1,mxatyp
     Do ib=ia,mxatyp

! number of the interaction by its rdf key

        kk=lstrdf(ib*(ib-1)/2+ia)

! only for valid interactions specified for a look up

        If (kk <= ntprdf .and. kk > 0) Then

           Write(nrite,"(/,'g(r): ',3x,'ParticleType ',i2,3x,'&',3x,           &
		        'ParticleType ',i2,/,/,/,9x,'r',8x,',',6x,'g(r)',8x,',',       &
				6x,'n(r)',/)") ia,ib
           Write(nrdfdt,"('ParticleType ',i2,2x,'&',2x,                        &
		                  'ParticleType ',i2,/,/,/)") ia,ib
		   Write(nrdfdt,1)		   
		   Write(nrdfdt,2)

1          Format('TITLE = "RADIAL DISTRIBUTION FUNCTIONS" ',/)
2          Format('Variables =',9x,'"r"',8x,',',6x,' "g(r)" ',/)

! normalisation factor

           factor=volm*dens(ia)*dens(ib)*Real(numrdf,wp)
           If (ia == ib .and. Nint(numtyp(ia)) > 1) &
              factor=factor*0.5_wp 

! running integration of rdf

           sum=0.0_wp

! loop over distances

           zero=.true.

           Do i=1,mxgrdf

              If (zero .and. i < (mxgrdf-3)) zero=(rdf(i+2,kk) <= 0.0_wp)

              rrr = (Real(i,wp)-0.5_wp)*delr
              dvol= 4.0_wp*pi*(delr*rrr*rrr+(delr**3)/12.0_wp)

              gofr= rdf(i,kk)/(factor*dvol)

              sum = sum + gofr*dvol*dens(ib)

! null it if less than 1.0e-6_wp

              If (gofr <= 1.0e-6_wp) Then
                 gofr1 = 0.0_wp
              Else
                 gofr1 = gofr
              End If

              If (sum <= 1.0e-6_wp) Then
                 sum1 = 0.0_wp
              Else
                 sum1 = sum
              End If

! print out information


              If (.not.zero) Write(nrite,"(3x,f10.4,1p,2(6x,e14.6))") rrr,gofr1,sum1
              Write(nrdfdt,'(14x,1p,e14.6,6x,e14.6)') rrr,gofr


           End Do

        End If

     End Do

  End Do

  Close(nrdfdt)

End Subroutine rdf_compute


