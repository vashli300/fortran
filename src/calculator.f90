subroutine confirmation()
    implicit none
    character :: input

    print *, "WOULD YOU LIKE TO CONTINUE? (y/n)"
    read (*,*) input
    
    select case (input)
        case('y')
            call menu()
        case('Y')
            call menu()  
        case default
            return
    end select
end subroutine

subroutine addition()
    implicit none
    real :: a, b, o

    write (*,*) "ENTER NUMBERS TO BE ADDED BELOW"
    read (*,*) a,b

    o = a + b

    print *, "SUMMATION =", o

    call confirmation()

end subroutine

subroutine subtraction()
    implicit none
    real :: a, b, o

    write (*,*) "ENTER NUMBERS TO BE SUBTRACTED BELOW"
    read (*,*) a,b

    o = a - b

    print *, "DIFFERENCE =", o

    call confirmation()

end subroutine

subroutine multiplication()
    implicit none
    real :: a, b, o

    write (*,*) "ENTER NUMBERS TO BE MULTIPLIED BELOW"
    read (*,*) a,b

    o = a * b

    print *, "PRODUCT =", o

    call confirmation()

end subroutine

subroutine division()
    implicit none
    real :: a, b, o

    write (*,*) "ENTER NUMBERS TO BE DIVIDED BELOW"
    read (*,*) a,b

    o = a / b

    print *, "QUOTIENT =", o

    call confirmation()

end subroutine

recursive subroutine menu()
    implicit none
    integer :: input
   
    write (*,*) "--------------------"
    write (*,*) "#.  PROCEDURE"
    write (*,*) "--------------------"
    write (*,*) "1.  ADDITION "
    write (*,*) "2.  SUBTRACTION "
    write (*,*) "3.  MULTIPLICATION "
    write (*,*) "4.  DIVISION "
    write (*,*) "5.  QUIT "
    write (*,*) "--------------------"
    write (*,*) "PLEASE SELECT AN OPERATION: "
    
    read (*,*) input

    select case (input)
        case (1)
            call addition()
        case (2)
            call subtraction()
        case (3)
            call multiplication()
        case (4)
            call division()
        case (5)
            return
        case default
            call menu()
    end select
end subroutine

program calculator
    implicit none

    write (*,*) "*************************************************************"
    write (*,*) "WELCOME TO THE INTERDIMENSIONAL FORTRAN-90 CALCULATION SYSTEM"
    write (*,*) "*************************************************************"

    call menu()

end program calculator


