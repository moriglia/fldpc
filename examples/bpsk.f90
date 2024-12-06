program bpsk
  use ldpc_decoder, only: TDecoder
  use io_fortran_lib, only: from_file
  use stdlib_stats_distribution_normal, only: rvs_normal
  implicit none

  type(TDecoder) :: decoder

  integer, allocatable :: edge_data(:,:)
  real, allocatable :: x(:)
  real, allocatable :: y(:)
  real(8), allocatable :: lappr(:)
  real(8), allocatable :: lappr_updated(:)
  
  logical, allocatable :: synd(:)
  real :: N0, sigma
  integer :: n_iter, n_err, i


  
  call from_file(file="examples/dvbs2ldpc0.500.csv", into=edge_data, header=.true.)
  decoder = TDecoder(&
       edge_data(1,1),   & ! where the number of edges is stored
       edge_data(2:, 2), & ! list of variable nodes (index is the id of the edge, value is the id of the node)
       edge_data(2:, 3))   ! list of check nodes    (index is the id of the edge, value is the id of the node)


  allocate(x(decoder%vnum)) ! Number of variable nodes
  allocate(y(decoder%vnum))
  allocate(lappr(decoder%vnum))
  allocate(lappr_updated(decoder%vnum))
  allocate(synd(decoder%cnum)) !number of check nodes
  
  ! I'm not doing it, but remember to set the seed in your simulations
  call random_number(x)

  do i = 1, decoder%vnum
     if (x(i) < 0.5) then
        x(i) = 1.0
     else
        x(i) = -1.0
     end if
  end do

  synd = decoder%word_to_synd((x<0))

  N0 = 10**(.21) ! SNR = -2.1dB (Being Es = 1)
  sigma = sqrt(N0/2) ! Being N0 the variance of the noise for both quadratures
  ! Remember to set a seed for the stdlib (e.g `use stdlib_random, only: std_seed => random_seed`)
  y = rvs_normal(loc=x, scale=sigma)

  lappr = 4*y/N0 ! = 2*y/sigma**2

  n_iter = 50 ! intent inout ==> output is the actual number of iterations
  call decoder%decode(lappr, lappr_updated, synd, n_iter)
  
  n_err = 0

  do i = 1, decoder%vnum - decoder%cnum ! only on information bits
     if ((lappr_updated(i)> 0) .neqv. (x(i) > 0)) then
        n_err = n_err + 1
     end if
  end do

  print '("N_ERR=", i4, 8x, "N_ITERS=", i2)', n_err, n_iter
  
end program bpsk
