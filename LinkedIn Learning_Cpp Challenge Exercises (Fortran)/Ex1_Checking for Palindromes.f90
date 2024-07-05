! Ex1: Check for Palindromes
! Require user to enter a string
! Check if the string entered reads the same backwards as forwards. Ignore case of the characters. Must include all characters in the string
! If yes --> Palindromes (e.g level, step onno pets)
! If no --> not Palindromes (e.g don't nod)
    
    PROGRAM Ex1_PalindromeCheck
    
        implicit none
        
        ! Step 0: Variable Declaration
        integer, parameter :: ASCII_minLowerCase = 97
        integer, parameter :: ASCII_maxLowerCase = 122
        integer, parameter :: ASCII_minUpperCase = 65
        integer, parameter :: ASCII_maxUpperCase = 90
        integer, parameter :: ASCIIUpperToLower = 32
        
        integer, parameter :: MaxStrLen = 10000
        character (len = MaxStrLen) :: InPutStr
        integer :: StrLen
        integer :: idx, end_idx
        
        logical :: IsPalindrome = .true.
        
        character :: EndChar
        
        ! Step 1: Get user input
        print '(A, I0, A)',  "Enter string (Max ", MaxStrLen, " no. of characters: "
        read (*, '(A)') InPutStr
        
        ! Step 2: Get length of string
        StrLen = len_trim(InPutStr)
        
        ! Step 3: Check string reads same backwards as forwards
        do idx = 1, StrLen/2
            end_idx = StrLen - idx + 1
            
            ! Check if char are upper case --> if yes, change to lower cases (need to manually compare ASCII value. No build in fn to do so)
            if (iachar(InPutStr(idx:idx)) >= ASCII_minUpperCase .and. iachar(InPutStr(idx:idx)) <= ASCII_maxUpperCase) then
                InPutStr(idx:idx) = achar(iachar(InPutStr(idx:idx)) + ASCIIUpperToLower) ! To change Uppercase to Lowercase
            end if
            
             if (iachar(InPutStr(end_idx:end_idx)) >= ASCII_minUpperCase .and. iachar(InPutStr(end_idx:end_idx)) <= ASCII_maxUpperCase) then
                InPutStr(end_idx:end_idx) = achar(iachar(InPutStr(end_idx:end_idx)) + ASCIIUpperToLower) ! To change Uppercase to Lowercase
             end if
             
            ! Check if front and rear characters match
            if(InPutStr(idx:idx) /= InPutStr(end_idx:end_idx)) then
                IsPalindrome = .false.
            end if
            
        end do
        
        ! Step 4: Output result
        if (IsPalindrome == .true.) then
            print *, "Sentence IS PALINDROME"
        else 
            print *, "Sentence NOT PALINDROME"
        end if
        
        read (*, '(A)') EndChar
        
    END PROGRAM Ex1_PalindromeCheck
