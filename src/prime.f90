subroutine confirmation()
    implicit none
    character :: input

    print *, "WOULD YOU LIKE TO CONTINUE? (y/n)"
    read (*,*) input
    
    select case (input)
        case('y')
            call sieve()  
        case('Y')
            call sieve()  
        case default
            return
    end select
end subroutine

subroutine sieve()
    implicit none
    real :: range
    integer :: i, j, k, n, l
    logical, dimension(:), allocatable :: prime

    write (*,*) "ENTER NUMBER TO CALCULATE TO:"
    read (*,*) range

    l = 0

    allocate(prime(2:int(range)))

    do i = 2, int(range)
        prime(i) = .true.
    end do

    do n = 2, int(sqrt(range))
        
        do j = n**2, int(range), n
            prime(j) = .false.
    
        end do

    end do

    write (*,*) "PRIME NUMBERS:"

    do k=2, int(range)

        if (prime(k) .eqv. .true.) then
            write (*,*) k
            l = l+1
        end if 

    end do

    deallocate(prime)

    write (*,*) "THE NUMBER OF PRIMES IS ",l 

    call confirmation()

end subroutine

recursive subroutine menu()
    implicit none
    integer :: input
   
    write (*,*) "--------------------"
    write (*,*) "#.  ROUTINE"
    write (*,*) "--------------------"
    write (*,*) "1.  DETERMINE PRIMES "
    write (*,*) "2.  QUIT " 
    write (*,*) "--------------------"
    write (*,*) "PLEASE SELECT AN OPERATION: "
    
    read (*,*) input

    select case (input)
        case (1)
            call sieve()
        case (2)
            return
        case default
            call menu()
    end select
end subroutine

program prime
    implicit none

    write (*,*) "************************************************************************"
    write (*,*) "WELCOME TO THE INTERDIMENSIONAL FORTRAN-90 PRIME DETERMINATION SYSTEM"
    write (*,*) "************************************************************************"

    call menu()
end program
