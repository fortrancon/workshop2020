! vectorspace.f90 --
!     Define a module for dealing with vector spaces in an abstract sense
!
!     The basic data type is an abstract vector, with the following
!     operations:
!     "+" - to add two vectors to get a new vector
!     "*" - to multiply a vector with a scalar
!     We can use an array of vectors as a collection: the operation ".in."
!     determines if a given vector is in that collection.
!
!     Note:
!     The multiplication does not work as I want, because I need a PASS attribute
!     and that conflicts with some restriction.
!
module vectorspaces
    implicit none

    type, abstract :: vector
    contains
        private
        procedure(addition), deferred                :: add
       ! This does not work, for some reason:
        procedure(multiplication), deferred, pass(b) :: multiply ! Note the "pass(b)" attribute!
        procedure(membership), deferred              :: is_member
        generic, public                              :: operator(+)    => add
        generic, public                              :: operator(*)    => multiply
        generic, public                              :: operator(.in.) => is_member
    end type vector

    abstract interface
        function addition( a, b )
            import :: vector

            class(vector), intent(in)  :: a, b
            class(vector), allocatable :: addition
        end function addition

        function multiplication( a, b )
            import :: vector

            real, intent(in)           :: a
            class(vector), intent(in)  :: b
            class(vector), allocatable :: multiplication
        end function multiplication

        function membership( a, b )
            import :: vector

            class(vector), intent(in)               :: a
            class(vector), intent(in), dimension(:) :: b
            logical                                 :: membership
        end function membership
    end interface
end module vectorspaces

!
! Test this with an actual implementation
!
module vectors_3d
    use vectorspaces

    type, extends(vector) :: vector_3d
        real, dimension(3) :: coords
    contains
        procedure :: add                => add_3d
        procedure, pass(b) :: multiply  => multiply_3d  !<== Note: pass(b) required!
        procedure :: is_member          => is_member_3d
    end type

    ! Define this explicitly
    !interface operator(*)
    !    module procedure multiply_3d
    !end interface

    interface assignment(=)
        module procedure assign_vector_3d
    end interface

contains
function add_3d( a, b )
    class(vector_3d), intent(in)  :: a
    class(vector), intent(in)     :: b
    class(vector), allocatable    :: add_3d

    allocate( add_3d, mold = a )

    !
    ! Clumsy, but that is a consequence of the strict typing in Fortran
    !
    select type (add_3d)
        type is (vector_3d)
            select type (b)
                type is (vector_3d)
                    add_3d%coords = a%coords + b%coords
                class default
                     add_3d%coords = [0.0, 0.0, 0.0] ! A run-time error is probably better
            end select
    end select
end function add_3d

function multiply_3d( a, b )
    real, intent(in)              :: a
    class(vector_3d), intent(in)  :: b
    class(vector), allocatable    :: multiply_3d

    allocate( multiply_3d, mold = b )

    select type (multiply_3d)
        type is (vector_3d)
            multiply_3d%coords = a * b%coords
    end select

end function multiply_3d

logical function is_member_3d( a, b )
    class(vector_3d), intent(in)               :: a
    class(vector), intent(in), dimension(:)    :: b

    integer                                    :: i

    is_member_3d = .false.

    select type (b)
        type is (vector_3d)
            do i = 1,size(b)
                if ( all( a%coords == b(i)%coords ) ) then
                    is_member_3d = .true.
                    exit
                endif
            enddo

        class default
            ! Always return false!
            is_member_3d = .false.
    end select
end function is_member_3d

! coords --
!     To print the resulting vector, use a "getter"
!
function coords( a )
    class(vector), intent(in) :: a
    real, dimension(3)        :: coords

    select type ( a )
        type is (vector_3d)
            coords = a%coords
        class default
            coords = 0.0
    end select
end function coords

! assign_vector_3d --
!     Overcome the discrepancy between vector and vector_3d
!
subroutine assign_vector_3d( a, b )
    class(vector_3d), intent(out) :: a
    class(vector), intent(in)     :: b

    select type ( b )
        type is (vector_3d)
            a%coords = b%coords
        class default
            a%coords = 0.0
    end select
end subroutine assign_vector_3d

end module vectors_3d

! Test program for the vector space modules
!
! Note:
!     To keep the program small, there is no facility to manipulate a collection
!     of vectors, but you could add that too.
!
program test_space
    use vectors_3d

    implicit none

    type(vector_3d)                :: a, b, c
    type(vector_3d), dimension(10) :: vectors

    a = vector_3d( [1.0, 1.0, 1.0] )
    b = vector_3d( [2.0, 2.0, 2.0] )
    c = vector_3d( [3.0, 3.0, 3.0] )

    vectors = vector_3d( [0.0, 0.0, 0.0] ) ! Primitive initialisation

    vectors(1) = a
    vectors(2) = b

    write(*,*) 'Is a in the collection? ', a .in. vectors
    write(*,*) 'Is c in the collection? ', c .in. vectors

    vectors(3) = a + b
    write(*,*) 'Added the sum of a and b - should be equal to c'
    write(*,*) 'Is c in the collection? ', c .in. vectors

    write(*,*) 'a + b  = ', coords(a + b)
    write(*,*) '10 * c = ', coords(10.0 * c)

end program test_space
