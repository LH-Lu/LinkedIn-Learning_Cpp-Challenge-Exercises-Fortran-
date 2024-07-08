! Ex2: Sorting Arrays
! As an exercise, implement a sorting algorithm to sort an int array.
    
! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
    MODULE sort_arrays
        
        implicit none
        
        public :: ArraySort
        
    CONTAINS
        subroutine ArraySort (arr, ArrSize, IsDescend)
        
            implicit none
        
            ! Declare arguments
            integer, intent (inout) :: arr(:)
            integer, intent (in) :: ArrSize
            logical, intent (in) :: IsDescend
            
            ! Initialise fn variables
            integer :: idx1, idx2, MinValue, MaxValue, MinValueIdx, MaxValueIdx
            integer :: LeftMostEntryIdx, RightMostEntryIdx, LeftMostEntry, RightMostEntry
            integer :: tempDescend
            
            ! Sort array (modified selection sort algo)
            do idx1 = 1, ArrSize/2
               ! Initialise for new pass
                MinValue = arr(idx1)
                MaxValue = arr(idx1)
                MinValueIdx = idx1
                MaxValueIdx = idx1
                
                RightMostEntry = arr(ArrSize - idx1 + 1)
                RightMostEntryIdx = ArrSize - idx1 + 1
                
                ! Finding Min and Max values
                do idx2 = idx1, ArrSize - idx1 + 1
                    if (arr(idx2) < MinValue) then
                        MinValue = arr(idx2)
                        MinValueIdx = idx2
                    end if
                    
                    if (arr(idx2) > MaxValue) then
                        MaxValue = arr(idx2)
                        MaxValueIdx = idx2
                    end if
                end do
                
                ! Swap Min Value and Left Most Entry value
                LeftMostEntryIdx = idx1
                LeftMostEntry = arr(LeftMostEntryIdx)
                arr(LeftMostEntryIdx) = MinValue
                arr(MinValueIdx) = LeftMostEntry
                
                if (MaxValueIdx == LeftMostEntryIdx) then ! Need to update MaxValueIdx if it moves
                    MaxValueIdx = MinValueIdx
                end if
                
                ! Swap Max Value and Right Most Entry value
                RightMostEntryIdx = ArrSize - idx1 + 1
                RightMostEntry = arr(RightMostEntryIdx)
                arr(RightMostEntryIdx) = MaxValue
                arr(MaxValueIdx) = RightMostEntry
                
            end do
            
            if (IsDescend == .false.) then
                return
            end if
            
            ! Rearrange array in descending order
            do idx1 = 1, ArrSize/2
                RightMostEntryIdx = ArrSize - idx1 + 1
                
                tempDescend = arr(idx1)
                arr(idx1) = arr(RightMostEntryIdx)
                arr(RightMostEntryIdx) = tempDescend
            end do
            
        end subroutine ArraySort
    
    END MODULE sort_arrays
    
    
    
    PROGRAM Ex2_SortArray
    
        use sort_arrays
        
        implicit none
        
        ! Initialise variables
        integer, parameter :: MAX_ARRAY = 100
        integer, dimension(MAX_ARRAY) :: arr
        integer :: ArrSize
        logical :: IsDescend
        integer :: IsDescendInt
        
        integer :: idx1
        
        character :: endchar
        
        ! Get user input
        print '(A, I0, A, \)', "Enter size of Array (MAX SIZE ", MAX_ARRAY, " ): "
        read (*, '(I)') ArrSize
        
        print *, "Enter array values: "
        do idx1 = 1, ArrSize
            read (*, '(I)') arr(idx1)
        end do
        
        print '(A, \)', "Sort the array in descending order? 0 - NO, 1 - YES: "
        read (*, '(I)') IsDescendInt
        
        select case (IsDescendInt)
        case (0)
            IsDescend = .false.
        case default
            IsDescend = .true.
        end select
        
        print '(A, \)', "The array entered is: "
        call print_arrays()
        
        ! Sort arrays
        call ArraySort(arr, ArrSize, IsDescend)
        
        ! Print result
        print '(A, \)', "The sorted array is: "
        call print_arrays()
        
        
        read(*, '(A)') endchar
        
    CONTAINS
        
    subroutine print_arrays()
            implicit none
            
            ! Initialise variables
            integer :: idx
            
            do idx = 1, ArrSize
                print '(I0, A,\)', arr(idx), " "
            end do
            
            print *, "" ! new line
            
    end subroutine print_arrays
        
    END PROGRAM Ex2_SortArray