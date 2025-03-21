module test_decoder_suite
  use iso_c_binding, only: wp => c_double
  use ldpc_decoder, only: TDecoder, process_vnode, process_cnode, llr_to_word
  use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
  use io_fortran_lib, only: from_file
  implicit none

  private

  public :: collect_suite


contains

  subroutine collect_suite(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [&
         new_unittest("TDecoderConstructor", test_ldpc_decoder_construction),&
         new_unittest("Test Destructor", test_destructor),&
         new_unittest("Node processing", test_processing),&
         new_unittest("Word to synd", test_word_to_synd),&
         new_unittest("LLR to word", test_llr_to_word),&
         new_unittest("Test decoding correct word", test_decode)&
         ]
  end subroutine collect_suite


  subroutine test_ldpc_decoder_construction(error)
    type(error_type), allocatable, intent(out) :: error

    type(TDecoder) :: decoder

    integer :: e_to_c(16)
    integer :: e_to_v(16)

    e_to_c = [0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4]
    e_to_v = [0, 2, 4, 7, 1, 3, 5, 0, 1, 5, 2, 6, 3, 4, 6, 7]


    decoder = TDecoder(16, e_to_v, e_to_c)

    call check(error, decoder%Ne, 16)
    if (allocated(error)) return

    call check(error, decoder%cnum, 5)
    if (allocated(error)) return

    call check(error, decoder%vnum, 8)
    if (allocated(error)) return

    call check(error, allocated(decoder%c_to_e))
    if (allocated(error)) return

    call check(error, allocated(decoder%c_to_v))
    if (allocated(error)) return

    call check(error, allocated(decoder%v_to_e))
    if (allocated(error)) return

    call check(error, allocated(decoder%v_to_c))
    if (allocated(error)) return

    call check(error, all(decoder%c_to_e(1)%data == [1, 2, 3, 4]))
    if (allocated(error)) return

    call check(error, all(decoder%c_to_v(1)%data == [1, 3, 5, 8]))
    if (allocated(error)) return

    call check(error, all(decoder%c_to_e(5)%data == [13, 14, 15, 16]))
    if (allocated(error)) return

    call check(error, all(decoder%c_to_v(5)%data == [4, 5, 7, 8]))
    if (allocated(error)) return

    call check(error, all(decoder%v_to_e(1)%data == [1, 8]))
    if (allocated(error)) return

    call check(error, all(decoder%v_to_c(1)%data == [1, 3]))
    if (allocated(error)) return

    call check(error, all(decoder%v_to_e(5)%data == [3, 14]))
    if (allocated(error)) return

    call check(error, all(decoder%v_to_c(5)%data == [1, 5]))
    if (allocated(error)) return

    call check(error, all(decoder%v_to_c(8)%data == [1,5]))
    if (allocated(error)) return

    call check(error, all(decoder%v_to_e(8)%data == [4,16]))
  end subroutine test_ldpc_decoder_construction


  subroutine test_destructor(error)
    type(error_type), allocatable, intent(out) :: error

    integer :: e_to_c(16)
    integer :: e_to_v(16)

    e_to_c = [0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4]
    e_to_v = [0, 2, 4, 7, 1, 3, 5, 0, 1, 5, 2, 6, 3, 4, 6, 7]


    block
      type(TDecoder) :: decoder
    end block !should not yield segfault

    block
      type(TDecoder) :: decoder
      decoder = TDecoder(16, e_to_v, e_to_c)
    end block
  end subroutine test_destructor

  subroutine test_processing(error)
    type(error_type), allocatable, intent(out) :: error

    type(TDecoder) :: decoder
    integer :: e_to_c(16)
    integer :: e_to_v(16)
    integer :: i

    real(wp) :: m_c_to_v(16)
    real(wp) :: m_v_to_c(16)
    real(wp) :: llr(8)
    real(wp) :: llr_updated(8)

    e_to_c = [0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4]
    e_to_v = [0, 2, 4, 7, 1, 3, 5, 0, 1, 5, 2, 6, 3, 4, 6, 7]
    decoder = TDecoder(16, e_to_v, e_to_c)

    m_c_to_v(:) = 0_wp
    m_v_to_c(:) = 0_wp

    llr(:) = 1.0_wp

    do i = 1, 8
       call process_vnode(decoder%v_to_e(i), llr(i), llr_updated(i), 16, m_v_to_c)
    end do

    call check(error, all(abs(m_v_to_c - 1.0_wp) < 0.001_wp))
    if (allocated(error)) return

    m_v_to_c = [(1.0_wp+real(i, wp)*0.1_wp, i = 1, 16)]
    m_c_to_v = m_v_to_c
    do i = 1, 5
       call process_cnode(decoder%c_to_e(i), .false., 16, m_c_to_v)
    end do

    call check(error, all(abs(m_c_to_v(1:4) - [0.37544_wp, 0.34937_wp, 0.32782_wp, 0.30979_wp]) < 0.001_wp))
    if (allocated(error)) return

    call check(error, all(abs(m_c_to_v(11:12) - [2.2_wp, 2.1_wp]) < 0.001_wp))
    if (allocated(error)) return

    m_c_to_v = m_v_to_c
    do i = 1, 5
       call process_cnode(decoder%c_to_e(i), .true., 16, m_c_to_v)
    end do

    call check(error, all(abs(m_c_to_v(1:4) + [0.37544_wp, 0.34937_wp, 0.32782_wp, 0.30979_wp]) < 0.001_wp))
    if (allocated(error)) return

    call check(error, all(abs(m_c_to_v(11:12) + [2.2_wp, 2.1_wp]) < 0.001_wp))
    if (allocated(error)) return
  end subroutine test_processing


  subroutine test_word_to_synd(error)
    type(error_type), allocatable, intent(out) :: error

    type(TDecoder) :: decoder
    integer :: e_to_c(16)
    integer :: e_to_v(16)

    logical :: word(8)
    logical :: synd(5)


    e_to_c = [0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4]
    e_to_v = [0, 2, 4, 7, 1, 3, 5, 0, 1, 5, 2, 6, 3, 4, 6, 7]
    decoder = TDecoder(16, e_to_v, e_to_c)

    word = [.true., .false., .true., .true., .false., .true., .false., .false.]

    synd = decoder%word_to_synd(word)

    call check(error, all(synd(:) .eqv. [.false., .false., .false., .true., .true.]))
  end subroutine test_word_to_synd


  subroutine test_llr_to_word(error)
    type(error_type), allocatable, intent(out) :: error

    real(wp) :: llr(8)
    logical :: word(8)

    llr = real([0.3, -0.9, 1.1, 9.2, -7.4, -5.4, 3.2, -1.1], wp)

    word = llr_to_word(8, llr)

    call check(error, all(word .eqv. [.false., .true., .false., .false., .true., .true., .false., .true.]))
  end subroutine test_llr_to_word


  subroutine test_decode(error)
    type(error_type), allocatable, intent(out) :: error

    type(TDecoder)        :: decoder
    integer, allocatable  :: edges(:,:)
    real(wp), allocatable :: x(:), lappr(:), lappr_upd(:)
    logical, allocatable  :: word(:), synd(:)

    integer :: i, N_iterations

    call from_file( &
         file="examples/dvbs2ldpc0.500.csv", &
         into=edges, &
         header=.true.)

    decoder = TDecoder( &
         edges(1,1),    &
         edges(2:, 2),  &
         edges(2:, 3))

    allocate(x(decoder%vnum))
    allocate(lappr(decoder%vnum))
    allocate(lappr_upd(decoder%vnum))
    allocate(word(decoder%vnum))
    allocate(synd(decoder%cnum))

    call random_number(x)

    do i = 1, decoder%vnum
       if (x(i) < 0.5) then
          x(i)    = 1
          word(i) = .false.
       else
          x(i)    = -1
          word(i) = .true.
       end if
    end do
    synd = decoder%word_to_synd(word)

    call random_number(lappr)
    lappr = lappr*0.5d+00 + x - 0.25d+00 ! |lappr - x| <= 0.25

    N_iterations = 50
    call decoder%decode(lappr, lappr_upd, synd, N_iterations)
    call check(error, N_iterations, 0)
    if (allocated(error)) return

    call check(error, all(lappr ==  lappr_upd))
  end subroutine test_decode

end module test_decoder_suite
