! events.f90 --
!     Module for handling events: to be used for "discrete event modelling"
!
!     The module defines two classes, basic_event and basic_event_stack, that have
!     a number of methods for storing and retrieving events. These classes should
!     be extended for an actual application. As each event must be a polymorphic
!     item, a dedicated derived type has been introduced.
!
!     TODO:
!     Make a simple extended class for a "real" application.
!
module events_library
    implicit none

    integer, parameter :: no_event_timestamp = -999

    type basic_event
        integer :: timestamp
    end type basic_event

    type basic_event_store
        class(basic_event), allocatable :: event
    end type basic_event_store

    type basic_event_stack
        type(basic_event_store), dimension(:), allocatable :: list_events
    contains
        procedure :: push      => push_event
        generic   :: pop       => pop_plain, pop_match
        procedure :: pop_plain => pop_event_plain
        procedure :: pop_match => pop_event_match
        procedure :: number    => number_events
    end type basic_event_stack

    private :: match_all

contains

! number_event --
!     Return the number of events
!
! Arguments:
!     this         Event stack to be used
!
integer function number_events( this )
    class(basic_event_stack), intent(in) :: this

    integer                              :: i

    number_events = 0
    do i = 1,size(this%list_events)
        if ( allocated(this%list_events(i)%event) ) then
            number_events = number_events + 1
        endif
    enddo
end function number_events

! push_event --
!     Push a new event on the stack
!
! Arguments:
!     this         Event stack to be used
!     event        Event to be added
!
! Note:
!     The timestamp of the event must be non-negative
!
subroutine push_event( this, event )
    class(basic_event_stack), intent(inout) :: this
    class(basic_event), intent(in)          :: event

    integer                  :: i ,idx

    if ( event%timestamp < 0 ) then
        stop 'Incorrect timestamp - must be positive'
    endif

    !
    ! To do: extend the array if necessary
    !
    if ( .not. allocated(this%list_events) ) then
        allocate( this%list_events(10000) )
    endif

    !
    ! Find an unused entry
    !
    idx = -1
    do i = 1,size(this%list_events)
        if ( .not. allocated(this%list_events(i)%event) ) then
            idx = i
            exit
        endif
    enddo

    if ( idx == -1 ) then
        ! Extend the array of events
        stop 'Too many events'
    endif

    !
    ! Copy the contents of the event into the array of events
    ! (this is done implicitly via the defined assignment)
    allocate( this%list_events(idx)%event, source = event )

end subroutine push_event

! pop_event_match --
!     Pop an event on the stack - the event with the _smallest_
!     timestamp that matches the criteria is popped
!
! Arguments:
!     this         Event stack to be used
!     match        Matching procedure
!     event        Event to be popped
!
subroutine pop_event_match( this, match, event )
    class(basic_event_stack), intent(inout)      :: this
    class(basic_event), allocatable, intent(out) :: event

    interface
        logical function match( event )
            import basic_event
            class(basic_event), intent(in) :: event
        end function match
    end interface

    integer :: i ,idx, minimal_time

    !
    ! Find the matching entry with the minimal timestamp
    !
    idx          = -1
    minimal_time = huge(1)
    do i = 1,size(this%list_events)
        if ( .not. allocated(this%list_events(i)%event) ) then
            cycle
        endif

        if ( match( this%list_events(i)%event ) ) then
            if ( minimal_time > this%list_events(i)%event%timestamp ) then
                idx = i
                minimal_time = this%list_events(i)%event%timestamp
            endif
        endif
    enddo

    !
    ! Pass the event back and remove it from the stack
    !
    if ( idx > -1 ) then
        if ( allocated(event) ) then
            deallocate( event )
        endif
        allocate( event, source = this%list_events(idx)%event )
        deallocate( this%list_events(idx)%event )
    else
        event%timestamp = no_event_timestamp
    endif
end subroutine pop_event_match

! pop_event_plain --
!     Pop an event on the stack - accepting any event
!
! Arguments:
!     this         Event stack to be used
!     event        Event to be popped
!
subroutine pop_event_plain( this, event )
    class(basic_event_stack), intent(inout)      :: this
    class(basic_event), allocatable, intent(out) :: event

    call this%pop( match_all, event )
end subroutine pop_event_plain

! match_all --
!     Just return .true. - accepting any event
!
! Arguments:
!     event        Event to be examined
!
logical function match_all( event )
    class(basic_event), intent(in)          :: event

    match_all = .true.
end function match_all

! valid_event --
!     Check if the event is a valid event or not
!
! Arguments:
!     event            Event to be checked
!
! Returns:
!     .true. if the timestamp is non-negative, otherwise .false.
!
logical function valid_event( event )
    class(basic_event), intent(in) :: event

    valid_event = event%timestamp >= 0
end function valid_event

end module events_library
