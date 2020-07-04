! queue_system.f90 --
!     Simulate a queueing system:
!     One queue, three booths at which the client is served
!     The booths handle a client and ask for the next one
!
!     Clients take some handling time:
!     33% take 1 minutes
!     33% take 2 minutes
!     33% take 5 minutes
!
module queue_events
    use events_library

    implicit none

    type, extends(basic_event) :: queue_event
        integer :: arrival_time   = -1   ! Time of arrival of the client
        integer :: handling_time  = -1   ! Time the client requires for handling their request
    end type queue_event

end module queue_events

program queue_system
    use queue_events
    implicit none

    type(basic_event_stack)         :: list_events
    class(queue_event), allocatable :: event
    class(basic_event), allocatable :: event_popped

    type booth_data
        integer :: clients_handled = 0
        integer :: idle_time       = 0
        integer :: current_time    = 0
    end type booth_data

    type(booth_data), dimension(3) :: booth

    integer                 :: i
    integer                 :: j
    integer, dimension(1)   :: loc
    real                    :: r
    integer                 :: number, average_number
    integer                 :: time, handling_time
    integer                 :: waiting_time
    integer                 :: cnt

    !
    ! Set up the queue: on average N clients per hour
    ! Their arrival is arbitrary over the hour
    ! There are eight working hours.
    !
    average_number = 20
    waiting_time   = 0

    allocate( event )

    !
    ! Random numbers are not always so random, unfortunately, use a
    ! naive procedure to get some randomness between runs even if a
    ! plain "call random_seed" does not provide this functionality.
    !
    call system_clock( cnt )
    call random_seed

    do i = 1,8
        do j = 1,mod(cnt,1000)
            call random_number( r )
        enddo
        number = 2.0 * r * average_number

        !
        ! Distribute them over the hour
        !
        do j = 1,number
            call random_number( r )
            time = (i-1) * 3600 + 60 * r

            call random_number( r )
            if ( r < 1.0/3.0 ) then
                handling_time = 60
            elseif ( r < 2.0/3.0 ) then
                handling_time = 180
            else
                handling_time = 300
            endif

            event%arrival_time  = time
            event%handling_time = handling_time

            call list_events%push( event )
        enddo
    enddo

    !
    ! Now we have three booths, as long as there are clients
    ! they need to be handled
    !
    number = list_events%number()

    do i = 1,number
        time = minval(booth%current_time)
        loc  = minloc(booth%current_time)
        j    = loc(1)

        call list_events%pop_plain( event_popped ) ! Get them in time order

        !
        ! Some trickery is required, it seems
        !
        select type (event_popped)
             class is (queue_event)
                 !call move_alloc( event, event_popped ) -- strange error message: event_popped shall be allocatable?
                 if ( allocated(event) ) then
                     deallocate( event )
                 endif
                 allocate( event, source = event_popped )
        end select
        !
        ! We have a client, handle them:
        ! - If the client arrived later than the ready time for the booth,
        !   the booth was idle.
        ! - Otherwise the client had to wait
        !
        if ( booth(j)%current_time < event%arrival_time ) then
            booth(j)%idle_time    = booth(j)%idle_time    + (event%arrival_time - booth(j)%current_time)
            booth(j)%current_time = event%arrival_time    + event%handling_time
        else
            waiting_time          = waiting_time          + (booth(j)%current_time - event%arrival_time)
            booth(j)%current_time = booth(j)%current_time + event%handling_time
        endif

        booth(j)%clients_handled = booth(j)%clients_handled + 1

!        write(*,*) j, booth(j)
!
!        if ( i > 4 ) then
!            exit
!        endif
    enddo

    !
    ! Output
    !
    write(*,'(a,i0)') 'Number of clients:  ', number
    write(*,'(a,i0)') 'Total waiting time: ', waiting_time
    do j = 1,size(booth)
        write(*,'(a,i0)') 'Booth ', j
        write(*,'(a,i0)') '    Number of clients: ', booth(j)%clients_handled
        write(*,'(a,i0)') '    Idle time:         ', booth(j)%idle_time
    enddo

end program queue_system
