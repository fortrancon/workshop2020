! vectorspace_function.f90 --
!     Define a module for dealing with vector spaces in an abstract sense
!     and a specific class for adding and multiplying functions.
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

            class(vector), intent(in), target :: a, b     !<= We need the target attribute here
            class(vector), allocatable        :: addition
        end function addition

        function multiplication( a, b )
            import :: vector

            real, intent(in)                  :: a
            class(vector), intent(in), target :: b
            class(vector), allocatable        :: multiplication
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
module vectors_function
    use vectorspaces

    type, extends(vector) :: vector_function
        real                               :: factor = 0.0
        procedure(f_of_x), nopass, pointer :: f  => null()
        type(vector_function), pointer     :: v1 => null()
        type(vector_function), pointer     :: v2 => null()
    contains
        procedure :: add               => add_functions
        procedure, pass(b) :: multiply => multiply_function  !<== Note: pass(b) required!
        procedure :: is_member         => is_member_dummy
        procedure :: eval              => eval_function
    end type

    interface assignment(=)
        module procedure assign_vector_function
    end interface

    interface
        real function f_of_x( x )
            real, intent(in) :: x
        end function f_of_x
    end interface

contains
function add_functions( a, b )
    class(vector_function), intent(in), target  :: a
    class(vector), intent(in), target           :: b
    class(vector), allocatable                  :: add_functions

    allocate( add_functions, mold = a )

    !
    ! Clumsy, but that is a consequence of the strict typing in Fortran
    !
    select type (add_functions)
        type is (vector_function)
            select type (b)
                type is (vector_function)
                    add_functions%v1 => a
                    add_functions%v2 => b
                class default
                     add_functions%v1 => null()
                     add_functions%v2 => null()
            end select
    end select
end function add_functions

function multiply_function( a, b )
    real, intent(in)                            :: a
    class(vector_function), intent(in), target  :: b
    class(vector), allocatable                  :: multiply_function

    allocate( multiply_function, mold = b )

    select type (multiply_function)
        type is (vector_function)
            multiply_function%factor =  a
            multiply_function%v1     => b
            multiply_function%v2     => null()
    end select

end function multiply_function

! Dummy - it is possible in principle, but you will have to descend the
!         tree of vectors
logical function is_member_dummy( a, b )
    class(vector_function), intent(in)         :: a
    class(vector), intent(in), dimension(:)    :: b

    is_member_dummy = .false.

end function is_member_dummy

! eval_function --
!     To evaluate the composite function for a particular value of x
!
recursive real function eval_function( a, x )
    class(vector_function), intent(in) :: a
    real                               :: x

    if ( associated(a%v2) ) then
        eval_function = a%v1%eval(x) + a%v2%eval(x)
    elseif ( associated(a%v1) ) then
        eval_function = a%factor * a%v1%eval(x)
    else if ( associated(a%f) ) then
        eval_function = a%f(x)
    else
        eval_function = 0.0
    endif
end function eval_function

! assign_vector_function --
!     Overcome the discrepancy between vector and vector_function
!
!     Note:
!     This is a rather naive implementation - we should actually
!     so a deep copy to make it robust.
!
subroutine assign_vector_function( a, b )
    class(vector_function), intent(out) :: a
    class(vector), intent(in)           :: b

    select type ( b )
        type is (vector_function)
            a%factor =  b%factor
            a%v1     => b%v1
            a%v2     => b%v2
        class default
            a%factor = 0.0
            a%v1     => null()
            a%v2     => null()
    end select
end subroutine assign_vector_function

subroutine setfunc( a, f )
    class(vector_function), intent(inout) :: a
    procedure(f_of_x), pointer            :: f

    a%f => f
end subroutine setfunc

end module vectors_function

! Test program for the vector space modules
!
program test_space
    use vectors_function

    implicit none

    type(vector_function)      :: a, b, c, d
    procedure(f_of_x), pointer :: f

    ! A bit roundabout, but f is a private member
    f => sine;   call setfunc( a, f )
    f => cosine; call setfunc( b, f )

    c = a + b
    d = 10.0 * c

    write(*,*) 'a at x = 1.0: ', a%eval(1.0)
    write(*,*) 'b at x = 1.0: ', b%eval(1.0)
    write(*,*) 'c at x = 1.0: ', c%eval(1.0)
    write(*,*) 'd at x = 1.0: ', d%eval(1.0)

contains
real function sine( x )
    real, intent(in) :: x
    sine = sin(x)
end function sine
real function cosine( x )
    real, intent(in) :: x
    cosine = cos(x)
end function cosine

end program test_space
