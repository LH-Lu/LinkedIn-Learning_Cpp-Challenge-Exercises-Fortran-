! Ex4: Check Bitonic Sequences
! Bitonic Sequences are sequences where there must be no more than one ascending and one descending order in the entire sequence, looped.
! For example,
! 1 2 3 4 3  is a bitonic sequence as it rises between the first and fourth element before descending to the last and back to the first element 
! (i.e ascend 1 -> 2 -> 3 -> 4    then descend 4 -> 3 -------> 1)
! 
! 3 2 3 4 1 is not bitonic b'cos there is two sets of ascending and two sets of descending order
! (i.e descend 3 -> 2    then ascend 2 -> 3 -> 4    descend 4 -> 1    then ascend 1 -------> 3)
!
! Note, regions of consantness between elements are not counted so 1 2 2 3 4 is also a bitonic sequence and 2 2 2 2 2 are also bitonic sequence (technically they're called monotonic sequence)
    
! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
    
PROGRAM Ex4_CheckBitonic

    implicit none
    
    ! Declare variables
    integer, parameter :: MAX_ARR_SIZE = 100
    integer, dimension(MAX_ARR_SIZE) :: arr
    integer :: ArrSize
    
    integer :: idx
    
    logical :: CheckBitonic
    
    character :: endchar
    
    ! Declare funcitons
    logical :: IsBitonic
    
    ! User input (length of sequence + contents)
    print '(A, I0, A, \)', "Enter length of array (MAX ARRAY LENGTH: ", MAX_ARR_SIZE, "): "
    read (*, '(I)') ArrSize
    
    print '(A)', "Enter array integer values: "
    do idx = 1, ArrSize
        read (*, '(I)') arr(idx)
    end do
    print *, " " ! new line
    
    print '(A,\)', "Entered array sequence is: "
    do idx = 1, ArrSize
        print '(I0, A, \)', arr(idx), " "
    end do
    print *, " " ! new line
    
    ! Check if bitonic
    CheckBitonic = IsBitonic(arr, ArrSize)
    
    ! Output result
    print '(A, \)', "The entered sequence is: "
    if (CheckBitonic == .true.) then
        print'(A, \)', "BITONIC"
    else
        print '(A, \)', "NOT BITONIC"
    end if
    
    print *, " " ! new line
    
    
    read (*, '(A)') endchar
    
END PROGRAM Ex4_CheckBitonic

    
    
FUNCTION IsBitonic (arr, ArrSize)
    
    implicit none
    
    ! Declare arguments
    integer, intent(in) :: arr(:)
    integer, intent(in) :: ArrSize
        
    ! Declare return
    logical :: IsBitonic
    
    ! Declare variables
    integer :: MinVal, MinIdx, MaxVal, MaxIdx
    
    ! Declare functions (DON'T NEED TO DECLARE FUNCTIONS THAT ARE CONTAINS)
    !logical :: IsContinuous
    
    ! Get Min and Max val
    call MinMaxVals ()
    
    ! Check if there is continuous decreases and increase between Max -> Min vals and Min -> Max vals respectively (ignoring constant)
    IsBitonic = IsContinuous(MinIdx, MaxIdx)
    if (IsBitonic == .false.) then
        return
    end if
    
    IsBitonic = IsContinuous(MaxIdx, MinIdx)
    
    
CONTAINS
        SUBROUTINE MinMaxVals()
            
            implicit none
            
            ! Declare variables
            integer :: MinMaxValsIdx
            
            ! Loop through array and find min / max values and their corresponding idx
            MinVal = arr(1)
            MaxVal = arr(1)
            MinIdx = 1
            MaxIdx = 1
            
            do MinMaxValsIdx = 2, ArrSize
                if (arr(MinMaxValsIdx) > MaxVal) then
                    MaxVal = arr(MinMaxValsIdx)
                    MaxIdx = MinMaxValsIdx
                end if
                
                if (arr(MinMaxValsIdx) < MinVal) then
                    MinVal = arr(MinMaxValsIdx)
                    MinIdx = MinMaxValsIdx
                end if
            end do
        
        END SUBROUTINE MinMaxVals
        
        
        FUNCTION IsContinuous (startIdx, endIdx)
            
            implicit none
            
            ! Declare arguments
            integer, intent(in) :: startIdx, endIdx
            
            ! Declare return
            logical :: IsContinuous
            
            ! Declare variables
            integer :: Idx, CompareIdx
            integer :: IncreaseCount, DecreaseCount, FailSafeCount
            integer, parameter :: MaxCount = 1
            integer, parameter :: FailSafe = 101
            logical :: check1 = .true.
            logical :: CheckContinuousIncrease, CheckContinuousDecrease
            
            ! Check number of increase / decrease count
            IncreaseCount = 0
            CheckContinuousIncrease = .false.
            
            DecreaseCount = 0
            CheckContinuousDecrease = .false.
            
            Idx = StartIdx
            CompareIdx = Idx + 1
            FailSafeCount = 0
            
            do while (check1)
                if (CompareIdx > ArrSize) then ! If idx is last element of array, need to compare with first element of array
                    CompareIdx = 1
                end if
                
                if (arr(Idx) > arr(CompareIdx) .and. CheckContinuousDecrease == .false.) then
                    DecreaseCount = DecreaseCount + 1
                    CheckContinuousDecrease = .true.
                    CheckContinuousIncrease = .false.
                end if
                
                if (arr(Idx) < arr(CompareIdx) .and. CheckContinuousIncrease == .false.) then
                    IncreaseCount = IncreaseCount + 1
                    CheckContinuousIncrease = .true.
                    CheckContinuousDecrease = .false.
                end if
                
                
                if (IncreaseCount > MaxCount .or. DecreaseCount > MaxCount) then
                    IsContinuous = .false.
                    return
                end if
                
                if (CompareIdx == EndIdx) then
                    check1 = .false.
                    exit
                end if
                
                if (FailSafeCount > FailSafe) then
                    IsContinuous = .false.
                    return
                    exit
                end if
                
                ! re-initialise for next pass
                if (CompareIdx == 1) then
                    Idx = 1
                else
                    Idx = Idx + 1
                end if
                
                CompareIdx = CompareIdx + 1
                FailSafeCount = FailSafeCount + 1
                
            end do
            
            IsContinuous = .true.
            
        END FUNCTION IsContinuous
    
END FUNCTION IsBitonic