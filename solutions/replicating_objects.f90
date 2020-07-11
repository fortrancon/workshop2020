! replicating_objects.f90 --
!      Objects that "disappear" (die) and replicate:
!      - an object has a certain chance to survive the time step
!      - if an object is old enough, it splits into two new objects
!
!      We start the simulation with a number of objects and stop
!      when a maximum is reached or when there are no more objects
!      alive.
!
!      Objects are characterised by their "age", the number of
!      time steps they have been active and the fact that they
!      are active (alive). We do not need to keep track of them
!      as independent items - we can simply use an array of the
!      right type and associated methods.
!
!      An overall object keeps track of the simulation.
!
!      All items have the same probability to survive and will
!      replicate at the same age, so these properties go with
!      simulation object.
!
module replication
    implicit none

    type item_data
        logical :: alive     ! Whether the item is alive
        integer :: age       ! The time it has been alive
    contains
        procedure :: die            => inactivate ! Inactivate the item
        procedure :: ready_to_spawn => old_enough ! Check if the item is old enough
        procedure :: create         => reset_item ! Renew an item
    end type item_data

    type simulation_data
        logical :: full      ! Whether the array of items reached full capacity or not
        integer :: maxage    ! Maximum age for items - at which they will spawn
        real    :: mortality ! The probability that an item will "die"
        type(item_data), dimension(:), allocatable :: item
    contains
        procedure :: setup    => setup_simulation
        procedure :: continue => continue_simulation
        procedure :: step     => step_simulation
        procedure :: report   => report_simulation
    end type simulation_data

contains

! inactivate --
!     Set the attribute "alive" to .false. and set the age to zero
!
! Arguments:
!     this             The item in question
!
subroutine inactivate( this )
    class(item_data), intent(inout) :: this

    this%alive = .false.
    this%age   = 0
end subroutine inactivate

! old_enough --
!     Determine whether the item is old enough
!
! Arguments:
!     this             The item in question
!     maxage           Maximum age
!
logical function old_enough( this, maxage )
    class(item_data), intent(in) :: this
    integer, intent(in)          :: maxage

    old_enough = this%age >= maxage
end function old_enough

! reset_item --
!     Reset the item
!
! Arguments:
!     this             The item in question
!
subroutine reset_item( this )
    class(item_data), intent(out) :: this

    this%alive = .true.
    this%age   = 0
end subroutine reset_item

! setup_simulation --
!     Set up the simulation
!
! Arguments:
!     this             The simulation object
!     maxnumber        Maximum number of items at any one time
!     initial          Initial number of items
!     maxage           Maximum age for items
!     mortality        Mortality rate for items
!
subroutine setup_simulation( simulation, maxnumber, initial, maxage, mortality )
    class(simulation_data), intent(inout) :: simulation
    integer, intent(in)                   :: maxnumber
    integer, intent(in)                   :: initial
    integer, intent(in)                   :: maxage
    real, intent(in)                      :: mortality

    integer                               :: i

    simulation%full      = .false.
    simulation%maxage    = maxage
    simulation%mortality = mortality
    allocate( simulation%item(maxnumber) )

    do i = 1,maxnumber
        call simulation%item(i)%die
    enddo

    do i = 1,initial
        call simulation%item(i)%create
    enddo
end subroutine setup_simulation

! continue_simulation --
!     Can the simulation continue? (Any objects alive? Room for more?)
!
! Arguments:
!     this             The simulation object
!     maxnumber        Maximum number of items at any one time
!     initial          Initial number of items
!     maxage           Maximum age for items
!     mortality        Mortality rate for items
!
logical function continue_simulation( simulation )
    class(simulation_data), intent(in) :: simulation

    continue_simulation = count( simulation%item%alive ) > 0 .and. .not. simulation%full
end function continue_simulation

! step_simulation --
!     Do a time step
!
! Arguments:
!     this             The simulation object
!
subroutine step_simulation( simulation )
    class(simulation_data), intent(inout) :: simulation

    integer                               :: i, j
    logical                               :: found
    real                                  :: p

    do i = 1,size(simulation%item)
        associate( item => simulation%item(i), other_items => simulation%item )
            if ( item%alive ) then
                item%age = item%age + 1

                if ( item%ready_to_spawn( simulation%maxage ) ) then
                    call item%create

                    !
                    ! Find a "dead" item
                    !
                    found = .false.
                    do j = 1,size(simulation%item)
                        if ( .not. other_items(j)%alive ) then
                            call other_items(j)%create
                            found = .true.
                            exit
                        endif
                    enddo

                    !
                    ! If no item could be created, stop the simulation
                    !
                    if ( .not. found ) then
                        simulation%full = .true.
                        exit
                    endif
                endif

                !
                ! Does the item survive?
                !
                call random_number( p )

                if ( p < simulation%mortality ) then
                    call item%die
                endif
            endif
         end associate
     enddo
end subroutine step_simulation

! report_simulation --
!     Write some statistics
!
! Arguments:
!     this             The simulation object
!     step             Simulation step
!
subroutine report_simulation( simulation, step )
    class(simulation_data), intent(in) :: simulation
    integer, intent(in)                :: step

    integer                            :: number_items, number_new

    number_items = count( simulation%item%alive )
    number_new   = count( simulation%item%alive .and. simulation%item%age == 0 )

    write(*,'(5i5)') step, number_items, number_new

end subroutine report_simulation

end module replication

! demo --
!     Demonstrate the program
!
program demo_replication
    use replication

    implicit none

    integer               :: i
    type(simulation_data) :: simulation

    call simulation%setup( 100, 10, 5, 0.118 )

    i = 0
    do while( simulation%continue() .and. i < 100 )
        i = i + 1

        call simulation%step
        call simulation%report( i )
    enddo
end program demo_replication
