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
!     Attempt without abstract type
!
module vectorspaces
    implicit none

    type :: vector
    end type vector

    interface operator(+)
        module procedure addition
    end interface

    interface operator(*)
        module procedure multiplication
    end interface

    interface operator(.in.)
        module procedure membership
    end interface

contains
function addition( a, b )
    class(vector), intent(in)  :: a
    class(vector), intent(in)  :: b
    class(vector), allocatable :: addition

    allocate( addition )
end function addition

! Note: the other way around, did not work :(
function multiplication( a, b )
    real, intent(in)           :: a
    class(vector), intent(in)  :: b
    class(vector), allocatable :: multiplication

    allocate( multiplication )
end function multiplication

function membership( a, b )
    class(vector), intent(in)               :: a
    class(vector), intent(in), dimension(:) :: b
    logical                                 :: membership

    membership = .false.
end function membership

end module vectorspaces

!
! Test this with an actual implementation
!
module vectors_3d
    use vectorspaces

   ! type, extends(vector) :: vector_3d
    type :: vector_3d
        real, dimension(3) :: coords
    end type

    interface operator(+)
        module procedure add_3d
    end interface

    interface operator(*)
        module procedure multiply_3d
    end interface

    interface operator(.in.)
        module procedure is_member_3d
    end interface

contains
function add_3d( a, b )
    type(vector_3d), intent(in)  :: a
    type(vector_3d), intent(in)  :: b
    type(vector_3d)              :: add_3d

    add_3d%coords = a%coords + b%coords
end function add_3d

function multiply_3d( a, b )
    real, intent(in)             :: a
    type(vector_3d), intent(in)  :: b
    type(vector_3d)              :: multiply_3d

    multiply_3d%coords = a * b%coords
end function multiply_3d

logical function is_member_3d( a, b )
    type(vector_3d), intent(in)               :: a
    type(vector_3d), intent(in), dimension(:) :: b

    integer                                   :: i

    is_member_3d = .false.

    do i = 1,size(b)
        if ( all( a%coords == b(i)%coords ) ) then
            is_member_3d = .true.
            exit
        endif
    enddo
end function is_member_3d

! coords --
!     To print the resulting vector, use a "getter"
!
function coords( a )
    type(vector_3d), intent(in) :: a
    real, dimension(3)          :: coords

    coords = a%coords
end function coords

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
