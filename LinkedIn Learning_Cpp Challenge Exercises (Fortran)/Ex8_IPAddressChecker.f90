! Ex8: IP Address Checker
! Create a program that checks whether an IP address is valid
! Assume IPv4 address format
! Therefore, for an IP address to be valid:
!		Needs to have four numbers
!		Each number must be between 0 and 255 (inclusive)
! Examples of valid IP address: 158.25.2.33 and 192.168.0.1
! Examples of invalid IP addresses: 100.325.4 and 12.B5-33 FF

! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
PROGRAM Ex8_IPChecker
    
    implicit none
    
    ! Declare variables
    integer, parameter :: MAX_IP_LENGTH = 100
    character (len = MAX_IP_LENGTH) :: IPaddress
    logical :: IsValidIP = .false.
    
    character :: endchar
    
    ! Get IP Address from user
    print '(A, \)', "Enter IP Address in IPv4 format: "
    read (*, '(A)') IPaddress
    
    ! Check if IP Address is valid
    IsValidIP = CheckIP(trim(IPaddress))
    
    ! Return result
    print '(A, \)', "IP Address Validity Status: "
    if (IsValidIP == .true.) then
        print '(A)', "VALID"
    else
        print '(A)', "INVALID"
    end if
    
    
    read (*, '(A)') endchar

    
CONTAINS
    FUNCTION CheckIP(IP)
        
        implicit none
        
        ! Declare argument
        character (len = *), intent(in) :: IP
        
        ! Declare return
        logical :: CheckIP
        
        ! Declare variables
        integer :: idx, IPlength, TokenVal, NumOfPeriods, DigitStartIdx, DigitEndIdx
        integer, parameter :: MinTokenVal = 0
        integer, parameter :: MaxTokenVal = 255
        integer, parameter :: ReqNumOfPeriods = 3
        character (len = *), parameter :: ValidChars = '0123456789.'
        
        ! Get length of IPAddress string
        IPlength = len(IP)
        
        ! Check if IP Address entered is only digits and periods 
        if (verify(IP, ValidChars) /= 0) then
            CheckIP = .false.
            return
        end if
        
        ! Loop through IP Address string. Check: 1) token value 2) number of token digits entered (i.e must have 4 numbers or 3 periods)
        NumOfPeriods = 0
        DigitStartIdx = 1
        DigitEndIdx = 1
        
        do idx = 1, IPlength
            if (IP(idx:idx) == '.' .or. idx == IPLength) then
                if (IP(idx:idx) == '.') then
                    NumOfPeriods = NumOfPeriods + 1
                    DigitEndIdx = idx - 1
                end if
                if (idx == IPLength) then
                    DigitEndIdx = idx
                end if
                
                read (IP(DigitStartIdx:DigitEndIdx), '(I)') TokenVal
                
                if (TokenVal < MinTokenVal .or. TokenVal > MaxTokenVal) then
                    CheckIP = .false.
                    return
                end if
                
                ! re-initialise for next round
                DigitStartIdx = idx + 1
            end if
        end do
        
        if (NumOfPeriods /= ReqNumOfPeriods) then
            CheckIP = .false.
            return
        end if
        
        CheckIP = .true.
        
    END FUNCTION CheckIP
END PROGRAM Ex8_IPChecker