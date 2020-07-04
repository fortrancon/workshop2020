! linkedlist.f90 --
!     Define a simple module for a linked list
!
!     To be able to add elements at the start of the list
!     (without having to define a special type), we misuse the
!     first element for this purpose.
!
!     Inserting a new element is a trifle subtle: you need to keep
!     track of where you are in the list.
!
module linkedlists
    implicit none

    integer, parameter :: HEAD = 0
    integer, parameter :: TAIL = huge(1)

    type element_data
        integer :: value
    end type element_data

    type linked_list
        type(element_data)         :: data
        type(linked_list), pointer :: next => null()
    contains
        procedure :: insert => insert_element
        procedure :: get    => get_element
        procedure :: remove => remove_element
        procedure :: print  => print_list
    end type linked_list

contains

! print_list --
!     Simply print the stored value to the screen and move to
!     the next element
!
!     Alternative implementations:
!     - Use recursion
!     - Use a type-bound subroutine to print the contained data
!
subroutine print_list( this )
    class(linked_list), intent(in), target :: this

    class(linked_list), pointer            :: next
    integer                                :: idx

    next => this%next ! Skip the first element - it functions as the "head"

    if ( .not. associated(next) ) then
        write(*,*) '(empty list)'
        return
    endif

    idx = 1

    do while ( associated(next) )
        write(*,*) idx, next%data%value    ! Better: a type-specific routine

        next => next%next
        idx = idx + 1
    enddo
end subroutine print_list

! insert_element --
!     Insert an element to the list at a given numerical position.
!     If the position is zero or negative it is inserted as the
!     first one, if it is beyond the last one, it is appended to
!     the list
!
!     You can use the parameters HEAD and TAIL to make this
!     explicit
!
subroutine insert_element( this, pos, element )
    class(linked_list), intent(inout), target :: this
    integer, intent(in)                       :: pos
    type(element_data), intent(in), target    :: element

    class(linked_list), pointer               :: next, prev
    class(linked_list), pointer               :: new_element
    integer                                   :: idx
    logical                                   :: found

    next => this%next ! Special treatment for the first element - it functions as the "head"

    if ( .not. associated(next) ) then
        allocate( this%next )
        this%next%data = element
        return
    endif

    idx   = 1
    found = .false.
    next  => this

    do while ( associated(next) )
        if ( idx == pos .or. pos < 1 ) then
            found = .true.
            allocate( new_element )
            new_element%next => next%next
            new_element%data =  element       ! This might require a user-defined assignment
            next%next        => new_element
            exit
        endif

        prev => next
        next => next%next
        idx = idx + 1
    enddo

    !
    ! The variable prev is pointing to the last element, so
    ! use that to append the new element
    !
    if ( .not. found ) then
        allocate( prev%next )
        prev%next%data = element
        return
    endif

end subroutine insert_element

! remove_element --
!     Remove an element from the list at a given numerical position.
!     If the position is zero or negative the first element is
!     removed, if it is beyond the last one, the last one is removed
!     the list
!
!     You can use the parameters HEAD and TAIL to make this
!     explicit
!
subroutine remove_element( this, pos )
    class(linked_list), intent(inout), target :: this
    integer, intent(in)                       :: pos

    class(linked_list), pointer               :: next, prev
    class(linked_list), pointer               :: old_element
    integer                                   :: idx
    logical                                   :: found

    !
    ! Special treatment if we want to remove the first element
    if ( pos <= 1 ) then
        if ( associated(this%next) ) then
            this%data =  this%next%data
            this%next => this%next%next
            return
        endif
    endif

    idx   = 1
    found = .false.
    next  => this%next
    prev  => this

    do while ( associated(next) )
        if ( idx == pos .or. .not. associated(next%next) ) then
            found = .true.
            old_element      => next
            prev%next        => next%next

            deallocate( old_element )
            ! How about deleting the data associated with the element?
            exit
        endif

        prev => next
        next => next%next
        idx = idx + 1
    enddo

end subroutine remove_element

! get_element --
!     Get an element from the list at a given numerical position.
!     If the position is zero or negative the first element is
!     returned, if it is beyond the last one, the last one is returned
!
!     You can use the parameters HEAD and TAIL to make this
!     explicit
!
!     Note:
!     Retrieving an element from an empty list is not detectable!
!
function get_element( this, pos )
    class(linked_list), intent(in), target :: this
    integer, intent(in)                    :: pos
    type(element_data)                     :: get_element

    class(linked_list), pointer            :: next, prev
    integer                                :: idx
    logical                                :: found

    !
    ! Special treatment for the first element
    !
    if ( pos <= 1 ) then
        get_element = this%data
        return
    endif

    idx   = 1
    found = .false.
    next  => this%next
    prev  => this

    do while ( associated(next) )
        if ( idx == pos .or. .not. associated(next%next) ) then
            found = .true.
            get_element = next%data
            exit
        endif

        prev => next
        next => next%next
        idx = idx + 1
    enddo

end function get_element

end module linkedlists

! test_lists
!     Test the linkedlists module
!
program test_lists
    use linkedlists

    implicit none

    type(linked_list)  :: list
    type(element_data) :: element

    !
    ! Insert a few data:
    ! Inserting them at the head should result in the order 3, 2, 1
    !
    element%value = 1; call list%insert( HEAD, element )
    element%value = 2; call list%insert( HEAD, element )
    element%value = 3; call list%insert( HEAD, element )

    write(*,*) 'Expected order: 3, 2, 1'
    call list%print

    !
    ! Add a few elements at the end
    element%value = 4; call list%insert( TAIL, element )
    element%value = 5; call list%insert( TAIL, element )

    write(*,*) 'Expected order: 3, 2, 1, 4, 5'
    call list%print

    !
    ! Insert an element in the middle
    element%value = 6; call list%insert( 4, element )

    write(*,*) 'Expected order: 3, 2, 1, 6, 4, 5'
    call list%print

    !
    ! Remove the elements at position 4 and at the head
    !
    call list%remove( 4 )
    call list%remove( HEAD )

    write(*,*) 'Expected order: 2, 1, 4, 5'
    call list%print

    !
    ! Get the second element
    !
    write(*,*) 'Element 2: ', list%get(2)
end program test_lists
