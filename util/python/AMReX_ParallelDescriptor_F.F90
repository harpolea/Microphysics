
module amrex_paralleldescriptor_module
  use iso_c_binding
  use amrex_fort_module
  implicit none

  interface amrex_pd_bcast
     module procedure amrex_pd_bcast_r
     module procedure amrex_pd_bcast_rv
     module procedure amrex_pd_bcast_r2v
     module procedure amrex_pd_bcast_r3v
  end interface amrex_pd_bcast

  private
  public :: amrex_pd_myproc, amrex_pd_nprocs, amrex_pd_ioprocessor, amrex_pd_ioprocessor_number, &
       amrex_pd_bcast, amrex_pd_wtime

contains

  integer function amrex_pd_myproc ()
    amrex_pd_myproc = 1
  end function amrex_pd_myproc

  integer function amrex_pd_nprocs ()
    amrex_pd_nprocs = 1
  end function amrex_pd_nprocs

  logical function amrex_pd_ioprocessor ()
    integer(c_int) :: i
    i = 1
    amrex_pd_ioprocessor = (i.ne.0)
  end function amrex_pd_ioprocessor

  integer function amrex_pd_ioprocessor_number ()
    amrex_pd_ioprocessor_number = 1
  end function amrex_pd_ioprocessor_number

  subroutine amrex_pd_bcast_r (x, a_root)
    real(amrex_real), target :: x
    integer, intent(in), optional :: a_root
    integer :: root
    real(amrex_real) :: r(1)
    if (present(a_root)) then
       root = a_root
    else
       root = 1
    end if
    if (root .eq. amrex_pd_myproc()) then
       r(1) = x
    end if
    if (root .ne. amrex_pd_myproc()) then
       x = r(1)
    end if
  end subroutine amrex_pd_bcast_r

  subroutine amrex_pd_bcast_rv (x, a_root)
    real(amrex_real) :: x(:)
    integer, intent(in), optional :: a_root
    integer :: root
    if (present(a_root)) then
       root = a_root
    else
       root = amrex_pd_ioprocessor_number()
    end if
  end subroutine amrex_pd_bcast_rv

  subroutine amrex_pd_bcast_r2v (x, a_root)
    real(amrex_real) :: x(:,:)
    integer, intent(in), optional :: a_root
    integer :: root
    if (present(a_root)) then
       root = a_root
    else
       root = amrex_pd_ioprocessor_number()
    end if
  end subroutine amrex_pd_bcast_r2v

  subroutine amrex_pd_bcast_r3v (x, a_root)
    real(amrex_real) :: x(:,:,:)
    integer, intent(in), optional :: a_root
    integer :: root
    if (present(a_root)) then
       root = a_root
    else
       root = amrex_pd_ioprocessor_number()
    end if
  end subroutine amrex_pd_bcast_r3v

  function amrex_pd_wtime () result(r)
    real(amrex_real) :: r
    r = 0
  end function amrex_pd_wtime

end module amrex_paralleldescriptor_module
