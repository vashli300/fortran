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

subroutine celToFar()
 implicit none
    real :: input, answer

    write (*,*) "ENTER TEMP IN DEG. CELSIUS:"
    read (*,*) input

    answer = input * 1.8 + 32
    write (*,*) "ANSWER IS ",answer," DEGREES FAHRENHEIT"

    call confirmation()

end subroutine

subroutine farToCel()
    implicit none
    real :: input, answer

    write (*,*) "ENTER TEMP IN DEG. FAHRENHEIT:"
    read (*,*) input

    answer = (input - 32) / 1.8
    write (*,*) "ANSWER IS ",answer," DEGREES CELSIUS"

    call confirmation()

end subroutine

recursive subroutine menu()
    implicit none
    integer :: input
   
    write (*,*) "--------------------"
    write (*,*) "#.  CONVERSION"
    write (*,*) "--------------------"
    write (*,*) "1.  CELSIUS TO FAHRENHEIT "
    write (*,*) "2.  FAHRENHEIT TO CELSIUS "
    write (*,*) "3.  QUIT " 
    write (*,*) "--------------------"
    write (*,*) "PLEASE SELECT AN OPERATION: "
    
    read (*,*) input

    select case (input)
        case (1)
            call celToFar()
        case (2)
            call farToCel()
        case (3)
            return
        case default
            call menu()
    end select
end subroutine

program temperature
    implicit none

    write (*,*) "************************************************************************"
    write (*,*) "WELCOME TO THE INTERDIMENSIONAL FORTRAN-90 TEMPERATURE CONVERSION SYSTEM"
    write (*,*) "************************************************************************"

    call menu()

end program temperature