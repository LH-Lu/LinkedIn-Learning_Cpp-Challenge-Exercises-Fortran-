! Ex7: Complex Number Calculator using a method analogous to C++ Class
! Create a class to perform complex number calculations
! Overload operators for addition, subtraction, multiplication and division
! Write a function to print a complex number
    
! Note to self, before and after running script, to prevent MS VS linking errors during compilation due to multiple files present: 
! Solution Explorer --> r/click on file name --> properties --> Toggle 'Exclude from build'
    
MODULE CplxNum

! Everything is public
    
    implicit none  

    ! Declare data type
    type Cplx
        real :: Re = 0.0
        real :: Im = 0.0
    end type Cplx
    
    ! Declare operator overloading
    interface operator (+)
        module procedure CplxAdd
    end interface operator (+)
    
    interface operator (-)
        module procedure CplxSubtract
    end interface operator (-)
    
    interface operator (*)
        module procedure CplxMultiply
    end interface operator (*)
    
    interface operator (/)
        module procedure CplxDivide
    end interface operator (/)
    
    private :: CplxAdd, CplxSubtract, CplxMultiply, CplxDivide
    
CONTAINS
    
    FUNCTION CplxAdd (a, b) result(val)
        implicit none
        ! Declare arguments
        type(Cplx), intent (in) :: a, b
        ! Declare return
        type(Cplx) :: val
        
        val%Re = a%Re + b%Re
        val%Im = a%Im + b%Im
    END FUNCTION CplxAdd
    
    FUNCTION CplxSubtract (a, b) result(val)
        implicit none
        ! Declare arguments
        type(Cplx), intent (in) :: a, b
        ! Declare return
        type(Cplx) :: val
        
        val%Re = a%Re - b%Re
        val%Im = a%Im - b%Im
    END FUNCTION CplxSubtract
    
    FUNCTION CplxMultiply (a, b) result(val)
        implicit none
        ! Declare arguments
        type(Cplx), intent (in) :: a, b
        ! Declare return
        type(Cplx) :: val
        
        val%Re = a%Re * b%Re - a%Im * b%Im
        val%Im = a%Re * b%Im + a%Im * b%Re
    END FUNCTION CplxMultiply
    
    FUNCTION CplxDivide (a, b) result(val)
        implicit none
        ! Declare arguments
        type(Cplx), intent (in) :: a, b
        ! Declare return
        type(Cplx) :: val
        ! Declare variables
        real :: denominator
        
        denominator = b%Re * b%Re + b%Im * b%Im
        
		val%Re = (a%Re * b%Re + a%Im * b%Im) / denominator;
		val%Im = (a%Im * b%Re - a%Re * b%Im) / denominator;
    END FUNCTION CplxDivide
    
END MODULE CplxNum
    
    
    
PROGRAM Ex7_FortranOOP

    ! Call module
    use CplxNum
    
    implicit none
    
    ! Declare variables
    type (Cplx) :: Complex1, Complex2, Complex3
    character :: ImSign
    character :: endchar
    
    ! Get user input
    print *, "Enter value of Complex Number 1: "
    print '(A,\)', "Real Component: "
    read (*, '(F)') Complex1%Re
    print '(A,\)', "Imaginary Component: "
    read (*, '(F)') Complex1%Im
    print *, " " ! new line
    
    print *, "Enter value of Complex Number 2: "
    print '(A,\)', "Real Component: "
    read (*, '(F)') Complex2%Re
    print '(A,\)', "Imaginary Component: "
    read (*, '(F)') Complex2%Im
    print *, " " ! new line
    
    Complex3 = Complex1 + Complex2
    call ImaginarySign (Complex3%Im)
    print '(A, F0.2, 3A, F0.2, A)', "Sum of complex numbers are: ", Complex3%Re , " ", ImSign, " ", Complex3%Im, "i"
    
    Complex3 = Complex1 - Complex2
    call ImaginarySign (Complex3%Im)
    print '(A, F0.2, 3A, F0.2, A)', "Difference of complex numbers are: ", Complex3%Re , " ", ImSign, " ", Complex3%Im, "i"
    
    Complex3 = Complex1 * Complex2
    call ImaginarySign (Complex3%Im)
    print '(A, F0.2, 3A, F0.2, A)', "Product of complex numbers are: ", Complex3%Re , " ", ImSign, " ", Complex3%Im, "i"
    
    Complex3 = Complex1 / Complex2
    call ImaginarySign (Complex3%Im)
    print '(A, F0.2, 3A, F0.2, A)', "Division of complex numbers are (Complex Number 1 / Complex Number 2): ", Complex3%Re , " ", ImSign, " ", Complex3%Im, "i"
    
    read (*, '(A)') endchar
    
CONTAINS
    
    SUBROUTINE ImaginarySign (ComplexIm)
        
        implicit none
        
        ! Declare argument
        real :: ComplexIm
        
        if (ComplexIm < 0) then
            ImSign = ''
        else 
            ImSign = '+'
        end if
        
    END SUBROUTINE ImaginarySign

END PROGRAM Ex7_FortranOOP