module array_module
    implicit none
    integer, dimension(:), allocatable :: global_array
    integer :: n

contains

recursive subroutine confirmation()
    implicit none
    character :: input

    print *, "QUIT PROGRAM? (y/n)"
    read (*,*) input
    
    select case (input)
        case('N')
            call menu()  
        case('n')
            call menu()  
        case default
            print *, "ARE YOU SURE? ARRAYS WILL BE DELETED! (y/n)"
            read (*,*) input

            select case (input)
                case('y')
                    return
                case('Y')
                    return
                case default
                    call menu()
            end select
    end select
end subroutine

recursive subroutine defineArray()
    implicit none
    integer :: i, iostat
    logical :: valid_input

    print *, "HOW LARGE SHOULD YOUR ARRAY BE?"
    read (*,*, iostat=iostat) n

    print *, "ARRAY SIZE =", n, "IOSTAT =", iostat

    if (iostat /= 0 .or. n <= 0) then
        print *, "SIGABRT ERR 101"
        return
    end if

    print *, "ARRAY ALLOCATION BEGINNING OF SIZE ", n
    if (allocated(global_array)) then
        deallocate(global_array, stat=iostat)
        if (iostat /= 0) then
            print *, "SIGABRT ERR 101 IOSTAT: ", iostat
            return
        end if 
       
    end if 

    allocate(global_array(n), stat=iostat)

    if (iostat /= 0) then
        print *, "SIGARBT ERR 102 IOSTAT: "
        return
    end if

    do i = 1, n
        valid_input = .false.
        do while (.not. valid_input)
            read (*,*, iostat=iostat) global_array(i)
            if (iostat /= 0) then
                print *, "INVALID INPUT FOR ", i
                print *, "SIGABRT ERR 101 RETRY IOSTAT: ", iostat
            else 
                valid_input = .true. 
            end if
        end do
    end do

    print *, "ARRAY ALLOCATED!"

    call menu()

end subroutine

recursive subroutine listArray()
    implicit none
    integer :: i

    if (.not. allocated(global_array)) then
        print *, "NO ARRAY TO LIST"
    else 
        print *, "ARRAY CONTENTS:"
        do i = 1, size(global_array)
            print *, global_array(i)
        end do
    end if

    call menu()
end subroutine

recursive subroutine deleteArray()
    implicit none
    character :: input
    
    print *, "ARE YOU SURE YOU WANT TO DELETE THIS ARRAY? THIS IS PERMANENT! (Y/N)"
    read (*,*) input

    select case (input)
        case ('Y')
            deallocate(global_array)
        case ('y')
            deallocate(global_array)
        case default
            print *, "ARRAY DELETION ABORTED"
    end select

    call confirmation()
    
end subroutine

recursive subroutine arrayOperationsMenu()
    implicit none
    integer :: input
   
    write (*,*) "--------------------"
    write (*,*) "#.  PROCEDURE"
    write (*,*) "--------------------"
    write (*,*) "1.  ARRAY SUM "
    write (*,*) "2.  ARRAY AVERAGE "
    write (*,*) "3.  ARRAY MINIMUM "
    write (*,*) "4.  ARRAY MAXIMUM "
    write (*,*) "5.  BACK "
    write (*,*) "--------------------"
    write (*,*) "PLEASE SELECT AN OPERATION: "
    
    read (*,*) input

    select case (input)
        case (1)
            call arraySum()
        case (2)
            call arrayAverage()
        case (3)
            call arrayMinimum()
        case (4)
            call arrayMaximum()
        case (5)
            call menu()
        case default
            call menu()
    end select

end subroutine

recursive subroutine arraySum()
    implicit none
    integer :: sum = 0
    integer :: i

    do i = 1, size(global_array)
        sum = sum + global_array(i)
    end do

    print *, "ARRAY SUM IS: ", sum

    call menu()

end subroutine

recursive subroutine arrayAverage()
    implicit none
    integer :: sum = 0
    integer :: i
    real :: average

    do i = 1, size(global_array)
        sum = sum + global_array(i)
    end do

    average = real(sum) / real(size(global_array))

    print *, "ARRAY AVERAGE IS: ", average

    call menu()

end subroutine

recursive subroutine arrayMinimum()
    implicit none
    integer :: min, i

    min = global_array(1)

    do i = 1, size(global_array)
        if (min > global_array(i)) then
            min = global_array(i)
        end if
    end do

    print *, "MINIMUM VALUE IS: ", min

    call menu()

end subroutine

recursive subroutine arrayMaximum()
    implicit none
    integer :: max, i

    max = global_array(1)

    do i = 1, size(global_array)
        if (max < global_array(i)) then
            max = global_array(i)
        end if
    end do

    print *, "MAXIMUM VALUE IS: ", max

    call menu()

end subroutine

recursive subroutine menu()
    implicit none
    integer :: input
   
    write (*,*) "--------------------"
    write (*,*) "#.  PROCEDURE"
    write (*,*) "--------------------"
    write (*,*) "1.  ARRAY DEFINITION "
    write (*,*) "2.  ARRAY LIST "
    write (*,*) "3.  ARRAY DELETION "
    write (*,*) "4.  ARRAY OPERATIONS "
    write (*,*) "5.  QUIT "
    write (*,*) "--------------------"
    write (*,*) "PLEASE SELECT AN OPERATION: "
    
    read (*,*) input

    select case (input)
        case (1)
            call defineArray()
        case (2)
            call listArray()
        case (3)
            call deleteArray()
        case (4)
            call arrayOperationsMenu()
        case (5)
            return
        case default
            call menu()
    end select
end subroutine

end module array_module

program array
    use array_module
    implicit none
    integer :: ierr

    write (*,*) "************************************************************************"
    write (*,*) "WELCOME TO THE INTERDIMENSIONAL FORTRAN-90 ARRAY MANIPULATION SYSTEM"
    write (*,*) "************************************************************************"

    call execute_command_line("ulimit -s unlimited", exitstat=ierr)
    if (ierr /= 0) then
        print *, "MAKE LARGE ARRAYS AND YOU ARE COOKED"
    end if
    
    call menu()

    if (allocated(global_array)) then
        deallocate(global_array, stat=ierr)
        if (ierr /= 0) then
            print *, "Warning: Error deallocating array at program end."
        end if
    end if

end program