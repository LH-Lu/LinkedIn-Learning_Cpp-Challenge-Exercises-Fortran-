! Ex6: Knight Chesspiece location predictor
! Write a program that takes in the chessboard location of a knight chess piece and prints the possible locations the knight can move to
! Assume:
!  The Knight chess piece is the only chess piece on the chessboard
! Note:
!  Knight can only move 2 squares horizontally + 1 square vertically OR 2 squares vertically + 1 sqaure horizontally --> Therefore, there are a maximum of 8 possible locations the Knight can move to
!  Chessboard grid layout is rows 1 - 8 (bottom to top) and A - H (left to right)

! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'

    
PROGRAM Ex6_HorsePlay

    implicit none
    
    ! Declare functions
    
    ! Declare variables
    integer, parameter :: MINROW = 1
    integer, parameter :: MAXROW = 8
    integer, parameter :: MINCOL = iachar('A')
    integer, parameter :: MAXCOL = iachar('H')
    integer, parameter :: ASCIILowerToUpper = -32
    
    integer :: InputRow
    character :: InputCol
    logical :: CheckInput = .false.
    
    character :: endchar
    
    ! Get Knight chess piece location on chess board and validate input
    do while (.not. CheckInput)
        print '(A)', "***KNIGHT CHESS PIECE MOVEMENT VALIDATOR***"
        
        CheckInput = InputKnightLocation()
        
        if (CheckInput == .false.) then
            print '(A)', "Wrong input!"
        end if
    end do
    print *, " " ! new line
    
    print '(A, A, I0)', "You have entered location: ", InputCol, InputRow
    
    ! Calculate possible locations Knight can move to + print results
    print '(A, \)', "The next possible location(s) is/are: "
    call PossibleLocationCalculator(2 ,1)
    call PossibleLocationCalculator(1, 2)
    
    print *, " " ! new line
    
    
    read (*, '(A)') endchar


CONTAINS
    
    FUNCTION InputKnightLocation()
        
        implicit none
        
        ! Declare arguements
        
        ! Declare return
        logical :: InputKnightLocation
        
        ! Declare variables
        integer :: ASCIIinputcol
        
        ! Get user input
        print '(A)', "Enter location of Knight chess piece: "
        print '(A, \)', "Enter Column Index (A - H): "
        read (*, '(A)') InputCol
        ASCIIinputcol = iachar(InputCol)
        print '(A, \)', "Enter Row Index (1 - 8): "
        read (*, '(I)') InputRow
       
        
        ! Validate user input + change col index to upper case
        if (InputRow < MINROW .or. InputRow > MAXROW) then
            InputKnightLocation = .false.
            return
        end if
        
        if (ASCIIinputcol >= iachar('a') .and. ASCIIinputcol <= iachar('z')) then ! convert any lower case input to upper case
            ASCIIinputcol = ASCIIinputcol + ASCIILowerToUpper
        end if
        
        if (ASCIIinputcol < MINCOL .or. ASCIIinputcol > MAXCOL) then
            InputKnightLocation = .false.
            return
        end if
        
        InputCol = achar(ASCIIinputcol)
        
        InputKnightLocation = .true.
    
    END FUNCTION InputKnightLocation
    
    
    SUBROUTINE PossibleLocationCalculator(VerticalMovement, HorizontalMovement)
        
        implicit none
        
        ! Declare arguments
        integer, intent(in) :: VerticalMovement
        integer, intent(in) :: HorizontalMovement
        
        ! Declare variables
        integer :: ASCIICol
        
        ! Check movements -> if yes, print result
        ASCIICol = iachar(InputCol)
        
        if (InputRow + VerticalMovement <= MAXROW) then
            
            if(ASCIICol + HorizontalMovement <= MAXCOL) then
                print '(A, I0, A, \)', achar(ASCIICol + HorizontalMovement), InputRow + VerticalMovement, " "
            end if
            
            if (ASCIICol - HorizontalMovement >= MINCOL) then
                print '(A, I0, A, \)', achar(ASCIICol - HorizontalMovement), InputRow + VerticalMovement, " "
            end if
            
        end if
        
        if (InputRow - VerticalMovement >= MINROW) then
            
            if(ASCIICol + HorizontalMovement <= MAXCOL) then
                print '(A, I0, A, \)', achar(ASCIICol + HorizontalMovement), InputRow - VerticalMovement, " "
            end if
            
            if (ASCIICol - HorizontalMovement >= MINCOL) then
                print '(A, I0, A, \)', achar(ASCIICol - HorizontalMovement), InputRow - VerticalMovement, " "
            end if
            
        end if
    
    END SUBROUTINE PossibleLocationCalculator


END PROGRAM Ex6_HorsePlay