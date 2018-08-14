
module amrex_error_module

  use iso_c_binding
  use amrex_string_module

  implicit none

  interface amrex_error
     module procedure amrex_error0
     module procedure amrex_error1_ch
     module procedure amrex_error1_i
     module procedure amrex_error1_r
  end interface amrex_error

  private

  public :: amrex_error, amrex_abort

contains

  subroutine amrex_error0 (message)
    character(len=*), intent(in) :: message
    write(*,*) message
  end subroutine amrex_error0

  subroutine amrex_error1_ch (message, str)
    character(len=*), intent(in) :: message, str
    write(*,*) message
  end subroutine amrex_error1_ch

  subroutine amrex_error1_i (message, i)
    character(len=*), intent(in) :: message
    integer, intent(in) :: i
    character(len=16) :: imsg
    write(imsg,*) i
    write(*,*) message
  end subroutine amrex_error1_i

  subroutine amrex_error1_r (message, r)
    use amrex_fort_module, only : amrex_real
    character(len=*), intent(in) :: message
    real(amrex_real), intent(in) :: r
    character(len=30) :: rmsg
    write(rmsg,*) r
    write(*,*) message
  end subroutine amrex_error1_r

  subroutine amrex_abort (message)
    character(len=*), intent(in) :: message
    write(*,*) message
  end subroutine amrex_abort

end module amrex_error_module
