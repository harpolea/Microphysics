! Common variables and routines for burners
! that use BS for their integration.

module bs_integrator_module

  implicit none

contains

  subroutine bs_integrator_init()

    use bs_type_module, only: nseq

    implicit none

    nseq = [2, 6, 10, 14, 22, 34, 50, 70]

    !$acc update device(nseq)

  end subroutine bs_integrator_init



  ! Main interface

  subroutine bs_integrator(state_in, state_out, dt, time, status)

    !$acc routine seq

    use extern_probin_module, only: burner_verbose
    use sdc_type_module, only: sdc_t
    use stiff_ode, only: ode
    use bs_type_module, only: bs_t, sdc_to_bs, bs_to_sdc
    use amrex_constants_module, only: ZERO
    use amrex_fort_module, only : rt => amrex_real
    use rpar_indices, only : irp_t0
    use integration_data, only: integration_status_t

    implicit none

    ! Input arguments

    type(sdc_t), intent(in   ) :: state_in
    type(sdc_t), intent(inout) :: state_out
    real(rt),  intent(in   ) :: dt, time
    type(integration_status_t), intent(inout) :: status

    ! Local variables
    integer :: ierr

    real(rt) :: t0, t1                   ! starting and ending time

    type (bs_t) :: bs

    real(rt) :: retry_change_factor

    ! BS does not allow for per-equation tolerances, so aggregate them here
    bs % atol(:) = 0.d0
    bs % rtol(:) = max(status % rtol_spec, status % rtol_temp, status % rtol_enuc)

    ! Initialize the integration time.
    t0 = ZERO
    t1 = t0 + dt

    call sdc_to_bs(state_in, bs)

    bs % n_rhs = 0
    bs % n_jac = 0

    bs % self_heat = .false.

    ! set the time offset -- we integrate from 0 to dt, so this
    ! is the offset to simulation time
    
    bs % u(irp_t0) = time

    ! Call the integration routine.

    call ode(bs, t0, t1, maxval(bs % rtol), ierr)
    
    ! Store the final data

    call bs_to_sdc(state_out, bs)

  end subroutine bs_integrator

end module bs_integrator_module
