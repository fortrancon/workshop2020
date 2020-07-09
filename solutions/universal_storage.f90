! universal_storage.f90 --
!     Module for storing data of "any" type
!
!     Taking a simple approach: there is simply an
!     array of fixed size within which we store the
!     data.
!
!     You can store the data in any element -- you
!     will have to keep track of the index yourself
!
!     Notes:
!     - The data that can be stored are *scalars*
!     - The get routines are, unfortunately, type-specific
!
module universal_storages
    implicit none

    type storage_element
        class(*), allocatable :: data
    end type storage_element

    type universal_storage
        type(storage_element), dimension(:), allocatable :: element
    contains
        procedure :: initialise => initialise_storage
        procedure :: add        => add_storage
        procedure :: get_int    => get_storage_int
        procedure :: get_real   => get_storage_real
        generic   :: get        => get_int, get_real
    end type universal_storage

contains

! initialise_storage --
!     Initialise the storage
!
! Arguments:
!     this             Storage object to be initialised
!     capacity         Number of data that can be stored
!
subroutine initialise_storage( this, capacity )
    class(universal_storage), intent(inout) :: this
    integer, intent(in)                     :: capacity

    allocate( this%element(capacity) )

end subroutine initialise_storage

! add_storage --
!     Add a data item to the storage
!
! Arguments:
!     this             Storage object
!     idx              Index at which to store the item
!     item             Data item to be stored
!
! Note:
!     Simple check on the index
!
subroutine add_storage( this, idx, item )
    class(universal_storage), intent(inout) :: this
    integer, intent(in)                     :: idx
    class(*), intent(in)                    :: item

    if ( .not. allocated(this%element) ) then
        write(*,*) 'Storage not initialised!'
        stop
    endif

    if ( idx < 1 .or. idx > size(this%element) ) then
        write(*,*) 'Index out of range for the storage!'
        stop
    endif

    if ( allocated( this%element(idx)%data ) ) then
        deallocate( this%element(idx)%data )
    endif

    !
    ! Use sourced allocation: adopt the dynamic type and copy the value
    !
    allocate( this%element(idx)%data, source = item )

end subroutine add_storage

! get_storage_real --
!     Get a real item from the storage
!
! Arguments:
!     this             Storage object
!     idx              Index at which the item was stored
!     item             Retrieved data item
!     success          Whether successful or not
!
! Note:
!     Simple check on the index
!
subroutine get_storage_real( this, idx, item, success )
    class(universal_storage), intent(inout) :: this
    integer, intent(in)                     :: idx
    real, intent(out)                       :: item
    logical, intent(out)                    :: success

    if ( .not. allocated(this%element) ) then
        write(*,*) 'Storage not initialised!'
        stop
    endif

    if ( idx < 1 .or. idx > size(this%element) ) then
        write(*,*) 'Index out of range for the storage!'
        stop
    endif

    ! No data item stored at given index
    if (.not. allocated( this%element(idx)%data ) ) then
        success = .false.
        return
    endif


    success = .true.

    select type ( v => this%element(idx)%data )
        type is (real)
            item = v
        class default
            success = .false. ! Wrong type
    end select

end subroutine get_storage_real


! get_storage_int --
!     Get an integer item from the storage
!
! Arguments:
!     this             Storage object
!     idx              Index at which the item was stored
!     item             Retrieved data item
!     success          Whether successful or not
!
! Note:
!     Simple check on the index
!
subroutine get_storage_int( this, idx, item, success )
    class(universal_storage), intent(inout) :: this
    integer, intent(in)                     :: idx
    integer, intent(out)                    :: item
    logical, intent(out)                    :: success

    if ( .not. allocated(this%element) ) then
        write(*,*) 'Storage not initialised!'
        stop
    endif

    if ( idx < 1 .or. idx > size(this%element) ) then
        write(*,*) 'Index out of range for the storage!'
        stop
    endif

    ! No data item stored at given index
    if (.not. allocated( this%element(idx)%data ) ) then
        success = .false.
        return
    endif


    success = .true.

    select type ( v => this%element(idx)%data )
        type is (integer)
            item = v
        class default
            success = .false. ! Wrong type
    end select

end subroutine get_storage_int

end module universal_storages

! test_storage
!     Simple test program
!
program test_storage
    use universal_storages

    implicit none

    type(universal_storage) :: storage
    integer                 :: i
    integer                 :: int_value
    real                    :: real_value
    logical                 :: success


    call storage%initialise( 20 )

    call storage%add( 1, 123 )
    call storage%add( 2, exp(1.0) )   ! Not the cliche value of pi ;)

    !
    ! We should be able to get an integer value from index 1,
    ! but not from any others
    ! And similarly for the real value at index 2.
    !

    do i = 1,3
        call storage%get( i, int_value, success )

        if ( success ) then
            write(*,*) 'Retrieved: ', int_value, ' - index:', i
        else
            write(*,*) 'No integer at index', i
        endif
    enddo

    do i = 1,3
        call storage%get( i, real_value, success )

        if ( success ) then
            write(*,*) 'Retrieved: ', real_value, ' - index:', i
        else
            write(*,*) 'No real at index', i
        endif
    enddo

end program test_storage
