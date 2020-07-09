! moving_average_alt.f90 --
!     Class for moving averages
!
!     Straightforward version:
!     Use a "growing" buffer
!
module moving_averages_alt
    use ieee_arithmetic

    implicit none

    type :: moving_average
        integer :: window_size  ! Maximum number of data in the window for averaging
        real, dimension(:), allocatable :: buffer
    contains
        procedure :: initialise => initialise_ma
        procedure :: add        => add_ma
        procedure :: average    => average_ma
    end type moving_average

contains

! initialise_ma --
!     Initialise the object for moving averages
!
! Arguments:
!     this          Object to be initialised
!     number        Window size
!
subroutine initialise_ma( this, number )
    class(moving_average), intent(inout) :: this
    integer, intent(in)                  :: number

    this%window_size = number

    !
    ! Clean up the buffer - do not reuse it for simplicity
    !
    if ( allocated( this%buffer ) ) then
        deallocate( this%buffer )
    endif

    allocate( this%buffer(0) )
end subroutine initialise_ma

! add_ma --
!     Add a new value to the object
!
! Arguments:
!     this          Object to be initialised
!     value         Value to be added
!
subroutine add_ma( this, value )
    class(moving_average), intent(inout) :: this
    real, intent(in)                     :: value

    if ( size(this%buffer) < this%window_size ) then
        this%buffer = [ this%buffer, value ]
    else
        this%buffer = [ this%buffer(2:), value ]
    endif
end subroutine add_ma

! average_ma --
!     Return the average of the current buffer - the moving average
!
! Arguments:
!     this          Object to be initialised
!
real function average_ma( this )
    class(moving_average), intent(inout) :: this

    if ( size(this%buffer) > 0 ) then
        average_ma = sum( this%buffer ) / size(this%buffer)
    else
        average_ma = ieee_value( average_ma, ieee_quiet_nan )
    endif
end function average_ma

end module moving_averages_alt

! test_ma
!     Simpel test program
!
program test_ma
    use moving_averages_alt

    implicit none

    type(moving_average) :: ma
    integer              :: i
    real                 :: value

    !
    ! If all values are the same, then the moving average must be the same
    !
    call ma%initialise( 10 )

    do i = 1,20
        value = 1.0
        call ma%add( value )
        write(*,*) i, ma%average()
    enddo

    !
    ! Now alternating values ... Re-initialise the object
    !
    call ma%initialise( 10 )

    do i = 1,20
        value = (-1.0) ** i
        call ma%add( value )
        write(*,*) i, ma%average()
    enddo

end program test_ma
