MODULE print_data
    
    implicit none
    ! Overloading the "dummy" function print_func.
    ! By just calling print_func, depending on the no. of arguments / type of argument, will re-direct to the appropiate function / sub-routine
    interface print_func
        module procedure int_print
        module procedure two_int_print
        module procedure float_print
    end interface print_func
    
CONTAINS
        SUBROUTINE int_print(x)
            implicit none            
            integer, intent (in) :: x  ! Declare argument MUST BE INTENT(IN)
            print '(A, I0)', "The single integer value is: ", x
        END SUBROUTINE int_print
        
        SUBROUTINE two_int_print(x, y)
            implicit none
            integer, intent (in) :: x, y  ! Declare argument MUST BE INTENT(IN)
            print '(A, I0, A, I0)', "The two integer values are: ", x, " and ", y
        END SUBROUTINE two_int_print
        
        SUBROUTINE float_print(x)
            implicit none
            real, intent (in) :: x  ! Declare argument MUST BE INTENT(IN)
            print '(A, F0.3)', "The single float value is: ", x
        END SUBROUTINE float_print
    
END MODULE print_data
    
PROGRAM main
    ! call module
    use print_data
    implicit none
    
    ! Declare variables
    integer :: IntVal1 = 3, IntVal2 = 8
    real :: FloatVal = 3.142
    character :: endchar
    
    ! Calling the overloaded function
    call print_func(IntVal1)
    call print_func(IntVal1, IntVal2)
    call print_func(FloatVal)
    
    read (*, '(A)'), endchar
    
END PROGRAM main
    