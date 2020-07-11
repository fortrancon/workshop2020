! thermostat.f90 --
!     Straightforward implementation of a thermostat system
!
module thermostat_simulation
    implicit none

    !
    ! Class for external temperatures
    !
    type temperature_data
        procedure(get_temperature), pointer, nopass :: get_temp
    contains
        procedure :: set  => set_procpointer
        procedure :: temp => temperature_value
    end type temperature_data

    abstract interface
        subroutine get_temperature( time, temp )
            implicit none
            real, intent(in)  :: time  ! Time in days
            real, intent(out) :: temp  ! Set temperature
        end subroutine get_temperature
    end interface

    !
    ! Class for the heating device
    !
    type heating_data
        real :: max_flux
    contains
        procedure :: hflux => heatflux
        procedure :: capacity => set_capacity
    end type heating_data

contains

! set_procpointer --
!     Store the pointer to the method to retrieve the set temperature
!
! Arguments:
!     this            Object for external temperatures
!     proc            Name of the subroutine to be used
!
subroutine set_procpointer( this, proc )
    class(temperature_data), intent(inout) :: this
    procedure(get_temperature)             :: proc

    this%get_temp => proc
end subroutine set_procpointer

! temperature_value --
!     Retrieve the temperature value as known by the object
!
! Arguments:
!     this            Object for external temperatures
!     time            Time in the simulation
!
real function temperature_value( this, time )
    class(temperature_data), intent(inout) :: this
    real, intent(in)                       :: time

    call this%get_temp( time, temperature_value )
end function temperature_value

! set_capacity --
!     Set the capacity for a heating device
!
! Arguments:
!     this            Heating device
!     capacity        Capacity of the device
!
subroutine set_capacity( this, capacity )
    class(heating_data), intent(inout) :: this
    real, intent(in)                   :: capacity

    this%max_flux = capacity
end subroutine set_capacity

! heatflux --
!     Determine the heat flux
!
! Arguments:
!     this            Heating device
!     tempdiff        Temperature difference
!
real function heatflux( this, tempdiff )
    class(heating_data), intent(inout) :: this
    real, intent(in)                   :: tempdiff

    heatflux = merge( this%max_flux, 0.0, tempdiff < 0.0 )
end function heatflux

end module thermostat_simulation


! demo_thermostat --
!     Demostration program:
!     - The room temperature to be achieved is a function of the time of day
!     - The outside temperature is a function of the time of year
!
program demo_thermostat
    use thermostat_simulation

    implicit none

    type(temperature_data) :: set_room_temp
    type(temperature_data) :: outside_temp
    type(heating_data)     :: heater

    real                   :: room_temp, dtemp
    real                   :: dt, time
    real                   :: hexch

    !
    ! Set the methods for retrieving the relevant temperature data
    !
    call set_room_temp%set( room_program )
    call outside_temp%set( seasonal_temperature )

    call heater%capacity( 12.5 )

    !
    ! Rather arbitrary value for the heat exchange coefficient with
    ! outside world. This value means that the room will cool down
    ! in about two days (0.5/day => 2 days)
    !
    hexch = 0.5
    !
    ! Run the simulation ...
    !
    room_temp = 0.0

    dt        = 0.06 / 24.0  ! six-minutes' time step
    time      = 0.0

    do while ( time < 20.0 )
         dtemp     = hexch * ( outside_temp%temp(time) - room_temp ) + heater%hflux( room_temp - set_room_temp%temp(time) )
         room_temp = room_temp + dt * dtemp

         write(*,*) time, room_temp, set_room_temp%temp(time), outside_temp%temp(time)

         time  = time + dt
    enddo

contains
subroutine room_program( time, temp )
    real, intent(in)  :: time
    real, intent(out) :: temp

    real              :: tday

    tday = 24.0 * mod(time, 1.0)  ! Time of day

    if ( tday < 7.0 .or. tday > 23.0 ) then
        temp =  0.0 ! Effectively no particular temperature set
    else
        temp = 19.0 ! Keep the room temperature at an agreeable level
    endif
end subroutine room_program

subroutine seasonal_temperature( time, temp )
    real, intent(in)  :: time
    real, intent(out) :: temp

    real              :: tday

    temp = 15.0 - 15.0 * cos( time / 365.0 )
end subroutine seasonal_temperature

end program demo_thermostat
