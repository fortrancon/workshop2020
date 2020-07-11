! abstract_framework.f90 --
!     Define a "framework" for solving a system of ordinary differential equations
!
!
module abstract_framework
    implicit none

    integer, parameter :: EULER = 1
    integer, parameter :: HEUN  = 2

    type, abstract :: ode_system_data
        ! Nothing in particular
    contains
        procedure(evaluate_rhs), deferred :: evaluate
    end type ode_system_data

    abstract interface
        function evaluate_rhs( this, time, x )
            import :: ode_system_data
            class(ode_system_data), intent(in)  :: this
            real, intent(in)                    :: time
            real, dimension(:), intent(in)      :: x
            real, dimension(size(x))            :: evaluate_rhs
        end function evaluate_rhs
    end interface

    type framework_data
        integer                             :: chosen_method ! Taking a shortcut
        integer                             :: luout          = -999
        real                                :: print_interval = -999.0
        class(ode_system_data), allocatable :: ode_system
    contains
        procedure :: method        => method_system
        procedure :: define        => define_system
        procedure :: print_options => print_options_system
        procedure :: solve         => solve_system
    end type framework_data

contains

! method_system --
!     Select a method for integration
!
! Arguments:
!     framework             The framework that should hold the system
!     method                Selected method ((EULER or HEUN)
!
! Note:
!     A bit of laziness, probably more in line with OO if you implement
!     it via a separate class or a procedure pointer.
!
subroutine method_system( framework, method )
    class(framework_data), intent(inout) :: framework
    integer, intent(in)                  :: method

    select case (method)
        case (EULER, HEUN)
            framework%chosen_method = method
        case default
            write(*,*) 'Unknown method: ', method, ' - using EULER'
            framework%chosen_method = EULER
    end select

end subroutine method_system

! print_options_system --
!     Set the output options
!
! Arguments:
!     framework             The framework that should hold the system
!     luout                 LU-number to write to
!     interval              Print interval
!
subroutine print_options_system( framework, luout, interval )
    class(framework_data), intent(inout) :: framework
    integer, intent(in)                  :: luout
    real, intent(in)                     :: interval

    framework%luout          = luout
    framework%print_interval = interval

end subroutine print_options_system

! define_system --
!     Store the object that defines the system of equations
!
! Arguments:
!     framework             The framework that should hold the system
!     system                The ODE system object
!
subroutine define_system( framework, system )
    class(framework_data), intent(inout) :: framework
    class(ode_system_data), intent(in)   :: system

    if ( allocated(framework%ode_system) ) then
        deallocate( framework%ode_system )
    endif

    allocate( framework%ode_system, source = system ) ! Safest: changes to the ODE system do not affect the framework

end subroutine define_system

! solve_system --
!     Actually solve the system, return the end result
!
! Arguments:
!     framework             The framework that should hold the system
!     tbegin                Start time
!     tend                  Stop time
!     dt                    Time step
!     xbegin                X-vector at start
!     xend                  X-vector at end
!
! Note:
!     Too lazy to implemen the Heun method
!
subroutine solve_system( framework, tbegin, tend, dt, xbegin, xend )
    class(framework_data), intent(inout) :: framework
    real, intent(in)                     :: tbegin, tend, dt
    real, dimension(:), intent(in)       :: xbegin
    real, dimension(:), intent(out)      :: xend

    real, dimension(size(xbegin))        :: dx
    real                                 :: time, time_print

    if ( framework%chosen_method /= EULER ) then
        write(*,*) 'Sorry, only EULER has been implemented'
        stop
    endif

    time       = tbegin
    time_print = tbegin
    xend = xbegin


    do while ( time < tend+0.5*dt )
        if ( abs(time - time_print ) < 0.5*dt ) then
            write( framework%luout, '(e12.4,5e12.4,/,(12x,5e12.4))' ) time, xend
            time_print = time_print + framework%print_interval
        endif

        if ( framework%chosen_method == EULER ) then
            dx = framework%ode_system%evaluate( time, xend )

            xend = xend + dt * dx
            time = time + dt
        endif
    enddo

end subroutine solve_system

end module abstract_framework


! oscilator --
!     Oscillator system
!
module oscillator
    use abstract_framework
    implicit none

    type, extends(ode_system_data) :: ode_oscillator
        real :: k, r
    contains
        procedure :: evaluate => evaluate_oscillator
    end type ode_oscillator

contains

! evaluate_oscillator --
!     Calculate the rhs for the oscillator system
!
function evaluate_oscillator( this, time, x )
    class(ode_oscillator), intent(in) :: this
    real, intent(in)                  :: time
    real, dimension(:), intent(in)    :: x
    real, dimension(size(x))          :: evaluate_oscillator

    evaluate_oscillator(1) = x(2)                             ! Velocity
    evaluate_oscillator(2) = - this%k * x(1) - this%r * x(2)  ! Acceleration
end function evaluate_oscillator

end module oscillator

! demo_framework --
!     Demonstrate the framework
!
program demo_framework
    use iso_fortran_env, only: output_unit
    use abstract_framework
    use oscillator

    implicit none

    type(ode_oscillator) :: damped_oscillator
    type(framework_data) :: framework
    real, dimension(2)   :: xbegin, xend

    !
    ! Define the system and solve it
    !
    damped_oscillator%k = 1.0
    damped_oscillator%r = 0.2

    call framework%define( damped_oscillator )
    call framework%method( EULER )
    call framework%print_options( output_unit, 0.1 )

    xbegin = [ 1.0, 0.0 ]
    call framework%solve( 0.0, 20.0, 0.01, xbegin, xend )

    write(*,*) 'Xbegin: ', xbegin
    write(*,*) 'Xend:   ', xend
end program demo_framework
