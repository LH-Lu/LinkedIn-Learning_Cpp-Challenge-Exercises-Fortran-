! Ex5: Recursive Factoral Sequence
! Write a recursive function to print sequence of factorials from 0! to n! 
! where n is a user generated integer between 1 to 10 (inclusive)
! Note,
!  Function to calculate the factorial must be recursive
!  Calculate each element only once (i.e dont call the recursive function n times for n!)
! 
! Example output from program: n = 4 --> print: {1, 1, 2, 6, 24} i.e {0!, 1!, 2!, 3!, 4!}
    
! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
    
PROGRAM Ex5_FactoralSequence

    implicit none
    
    ! Initialise variables
    integer :: n, TempVal
    character :: endchar
    
    ! Get value of n from user
    print '(A, \)', "Enter value of n: "
    read (*, '(I)') n
    
    ! Enter recursive function --> calculates and prints each factorial of the sequence
    TempVal = Factoral (n)
    print*, " " ! new line
    
    
    read (*, '(A)') endchar

CONTAINS
    
    RECURSIVE FUNCTION Factoral (n)
        
        implicit none
        
        ! Declare argument
        integer, intent(in) :: n
        
        ! Declare return
        integer :: Factoral
        
        ! Declare variables
        integer :: FacValue
        
        ! Calculate factoral
        if (n == 0) then ! Base Case
            Factoral = 1
        else ! Recursive case
            Factoral = n * Factoral(n - 1)
        end if
        
        print '(I0, A, \)', Factoral, " "
    
    END FUNCTION Factoral

END PROGRAM Ex5_FactoralSequence