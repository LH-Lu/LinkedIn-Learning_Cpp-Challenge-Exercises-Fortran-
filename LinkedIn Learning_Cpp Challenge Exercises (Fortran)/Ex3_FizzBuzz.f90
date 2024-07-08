! Ex3: Fizz Buzz
!  Fizz Buzz game
! Program prints sequence of int from 1 to n (n is user defined positive integer)
! But multiples of 3 replaced with "Fizz"
!  multiples of 5 replaced with "Buzz"
!  multiples of 3 and 5 replaced with "FizzBuzz"
    
! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
PROGRAM Ex3_FizzBuzz

    implicit none
    
    ! Initialise sub-fn
    logical :: IsMultiple
    
    ! Initialise Variables
    integer :: n, idx
    logical :: MultipleCheck
    character :: endchar
    
    ! User input value of n
    print '(A, \)', "Enter a value of n: "
    read (*, '(I)') n
    
    ! Print required output
    MultipleCheck = .false.
    do idx = 1, n
        MultipleCheck = IsMultiple(idx, 3, "Fizz", MultipleCheck)
        MultipleCheck = IsMultiple(idx, 5, "Buzz", MultipleCheck)
        
        if (MultipleCheck == .false.) then
            print '(I0,\)', idx
        end if
        
        print *, "" ! New line
        MultipleCheck = .false. ! Re-initialise for next round
        
    end do
    
    
    read (*, '(A)') endchar

END PROGRAM
    
! If value is a multiple, print OutputStr and return TRUE. Also return TRUE is PrvCheck is TRUE
FUNCTION IsMultiple(value, multiple, OutputStr, PrvCheck)
    
    implicit none
    
    ! Declare arguments
    integer, intent(in) :: value, multiple
    character(len=*), intent (in) :: OutputStr
    logical, intent(in) :: PrvCheck
    
    ! Declare return
    logical :: IsMultiple
    
    ! Declare variables
    
    
    ! Check if the value is a multiple of the arg multiple. If so, print req string
    if (mod(value, multiple) == 0) then
        print '(A, \)', trim(OutputStr)
        IsMultiple = .true.
        return
    end if
    
    if (PrvCheck == .true.) then
        IsMultiple = .true.
        return
    end if
    
    IsMultiple = .false.

END FUNCTION IsMultiple