Subroutine error(kode)

!    %--------------------------------------------------------------%
!    |  Subroutine for printing error messages and bringing about   |
!    |  a controlled termination of the program                     |
!    %--------------------------------------------------------------%

  Use setup_module,   Only : nrite

  Implicit None

  Integer, Intent( In    ) :: kode

  Write(nrite,'(/,1x,a,i5)') 'The program terminated due to error ', kode
	
  If      (kode ==    1) Then


  Else If (kode ==    2) Then
      Write(nrite,'(/,1x,a)') 'error - too many atoms specified in input_routine'
  
  Else If (kode ==   14) Then

      Write(nrite,'(/,1x,a)') 'error - too many unique atom types specified'	

  Else If (kode ==   20) Then

      Write(nrite,'(/,1x,a)') 'error - link cell algorithm violation'
  
  Else If (kode ==   25) Then



  Else If (kode ==   30) Then

      Write(nrite,'(/,1x,a)') 'error - too many link cells requested'

  Else If (kode ==   40) Then

      Write(nrite,'(/,1x,a)') 'error - neighbour list array too small in link_cell_pairs'

  Else If (kode ==   46) Then

	  Write(nrite,'(/,1x,a)') 'error - undefined direction passed to export_data'

  Else If (kode ==   56) Then

      Write(nrite,'(/,1x,a)') 'error - coordinate array exceeded in export_atomic_data'

  Else If (kode ==  100) Then

      Write(nrite,'(/,1x,a)') 'error - allocation failure in config_module -> allocate_config_arrays'

  Else If (kode ==  105) Then

      Write(nrite,'(/,1x,a)') 'error - allocation failure in param_module -> allocate_param_arrays'

  Else If (kode ==  109) Then

      Write(nrite,'(/,1x,a)') 'error - calculated pair rdf index too large -> allocate_param_arrays'
 
  Else If (kode ==  110) Then

      Write(nrite,'(/,1x,a)') 'error - duplicate rdf look up pair specified'

  Else If (kode ==  120) Then

      Write(nrite,'(/,1x,a)') 'error - failure in input viscosity'

  Else If (kode ==  454) Then

      Write(nrite,'(/,1x,a)') 'error - undefined external field'

  Else If (kode ==  519) Then

      Write(nrite,'(/,1x,a)') 'error - RESTART is incompatible'

  Else If (kode == 1016) Then

      Write(nrite,'(/,1x,a)') 'error - allocation failure in statistics_module -> allocate_statitics_arrays'

  Else If (kode == 1017) Then

      Write(nrite,'(/,1x,a)') 'error - allocation failure in density_module -> allocate_density_arrays'

   Else If (kode == 1019) Then

      Write(nrite,'(/,1x,a)') 'error - allocation failure in external_field_module -> allocate_external_field_arrays'

  Else

      Write(nrite,'(/,1x,a)') 'error - unnamed error found'

  End If

! close all i/o channels

  Close(nrite)

  STOP

End Subroutine error

!***************************************************************************

Subroutine warning(kode,a,b,c)

!    %--------------------------------------------------------------%
!    |  subroutine for printing warning messages and returning      |
!    |  control back to the main program                            |
!    %--------------------------------------------------------------%


  Use kinds_f90
  Use setup_module

  Implicit None

  Integer,           Intent( In    ) :: kode

  Real( Kind = wp ), Intent( In    ) :: a,b,c

  Integer                            :: ia,ib,ic


  Write(nrite,'(/,1x,a,i6)') 'warning issued ',kode

  If      (kode ==   2) Then

      ia = Nint(a)
      ib = Nint(b)

      Write(nrite,'(/,1x,a,2(i8,a))') &
      '*** warning - atoms required array size ', ia, ' and actual: ', ib, ' !!! ***'

  Else If (kode ==   3) Then

      Write(nrite,'(/,1x,2(a,f12.6),a)') &
      '*** warning - system cutoff is, ', a,',minimum half-cell width is,',b,' !!! ***'

  Else If (kode ==   5) Then

      Write(nrite,'(/,1x,a,f12.4,a)') &
      '*** warning - non-zero total system charge: ', a, ' !!! ***'

  Else If (kode ==  25) Then

      Write(nrite,'(2(/,1x,a))') &
      '*** warning - employed link cell algorithm has a link cell dimension that is < 4 !!! ***'

  Else If (kode ==  30) Then

      ia=Nint(a)
      ib=Nint(b)

      Write(nrite,'(2(/,1x,a),2(i6,a))') &
      '*** warning - cannot activate link cell option ***', &
      '*** more link-cells ', ia,' than allowed ', ib,' !!! ***'

  Else If (kode ==  40) Then

      ia = Nint(a)
      ib = Nint(b)

      Write (nrite,'(/,1x,a,2(i8,a))') &
      '*** warning - required link-cell list size is ', ia, ' and actual (mxlist) ', ib, ' !!! ***'

  Else If (kode == 110) Then

      Write(nrite,'(/,1x,a)') '*** warning - no pair forces in use ***'

  Else If (kode == 120) Then

      Write(nrite,'(/,1x,a,f12.6,a,i4,a)') &
      '*** warning - Schmit number ', a , ' is less than',int(b),' !!! ***'

  Else If (kode == 160) Then

      ia = Nint(a)
      ib = Nint(b)

      Write(nrite,'(/,1x,a,2(i8,a))') &
      '*** warning - required export array size ', ia, ' and actual: ', ib, ' !!! ***'

  Else If (kode == 190) Then

      Write(nrite,'(2(/,1x,a))') &
      '*** warning - RESTART format mismash detected (restart requested) ***', &
      '*** restart is abandoned and clean start is assumed ***'

  Else

      Write(nrite,'(/,1x,a)') &
      '*** unspecified warning encountered ***'

  End If


End Subroutine warning