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
    
    real :: a
    character :: endchar
    
    read (*, *) a
    print '(F)', a
    
    read (*, '(A)') endchar



END PROGRAM Ex8_IPChecker