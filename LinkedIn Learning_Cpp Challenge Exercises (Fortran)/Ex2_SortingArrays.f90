! Ex2: Sorting Arrays
! As an exercise, implement a simple sorting algorithm (not the one done in C++ version) to sort an int array in ascending order only
    
! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
    MODULE sort_arrays
        
        implicit none
        
    CONTAINS
        subroutine ArraySort (arr, ArrSize)
        
            implicit none
        
            ! Declare arguments
            integer, intent (inout) :: arr(:)
            integer, intent (in) :: ArrSize
            
            ! Initialise fn variables
            integer :: idx, idx2, MinVal, MinIdx
            
            ! Sort array (selection sort algo)
            do idx = 1, ArrSize
                MinVal = arr(idx)
                MinIdx = idx
                
                do idx2 = idx, ArrSize
                    if (arr(idx2) < MinVal) then
                        MinVal = arr(idx2)
                        MinIdx = idx2
                    end if
                    
                end do
                
                arr(MinIdx) = arr(idx)
                arr(idx) = MinVal
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
        
        integer :: idx1
        
        character :: endchar
        
        ! Get user input
        print '(A, I0, A, \)', "Enter size of Array (MAX SIZE ", MAX_ARRAY, " ): "
        read (*, '(I)') ArrSize
        
        print *, "Enter array values: "
        do idx1 = 1, ArrSize
            read (*, '(I)') arr(idx1)
        end do
        
        print '(A, \)', "The array entered is: "
        call print_arrays()
        
        ! Sort arrays
        call ArraySort(arr, ArrSize)
        
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