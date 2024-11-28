module matrix_module
    implicit none
    integer, dimension(:,:), allocatable :: matrix1, matrix2, matrixProduct
    integer :: MATRIX_1_ROW, MATRIX_1_COLUMN, MATRIX_2_ROW, MATRIX_2_COLUMN

contains

recursive subroutine input()
    implicit none
    integer :: i, j

    print *, "ENTER ROWS OF FIRST MATRIX"
    read (*,*) MATRIX_1_ROW

    print *, "ENTER COLUMNS OF FIRST MATRIX"
    read (*,*) MATRIX_1_COLUMN

    allocate(matrix1(MATRIX_1_ROW,MATRIX_1_COLUMN))

    print *, "ENTER THE NUMBERS FOR MATRIX ONE, ROW BY ROW, RIGHT TO LEFT "

    do i = 1, MATRIX_1_ROW
        do j = 1, MATRIX_1_COLUMN
            read (*,*) matrix1(i,j)
        end do
    end do

    print *, "ENTER ROWS OF SECOND MATRIX"
    read (*,*) MATRIX_2_ROW

    print *, "ENTER COLUMNS OF SECOND MATRIX"
    read (*,*) MATRIX_2_COLUMN

    allocate(matrix2(MATRIX_2_ROW,MATRIX_2_COLUMN))

    print *, "ENTER THE NUMBERS FOR MATRIX TWO, ROW BY ROW, RIGHT TO LEFT "

    do i = 1, MATRIX_2_ROW
        do j = 1, MATRIX_2_COLUMN
            read (*,*) matrix2(i,j)
        end do
    end do

    call multiply()

end subroutine

recursive subroutine multiply()
    implicit none
    character confirmation
    integer :: i, j, k

    if (MATRIX_1_COLUMN /= MATRIX_2_ROW) then
        print *, "YOURE TRASH"
        return
    end if

    allocate(matrixProduct(MATRIX_1_ROW,MATRIX_2_COLUMN))
    
    matrixProduct = 0


    do i = 1, MATRIX_1_ROW
        do j = 1, MATRIX_2_COLUMN
            do k = 1, MATRIX_1_COLUMN
                matrixProduct(i,j) = matrixProduct(i,j) + matrix1(i,k) * matrix2(k,j)
            end do
        end do
    end do

    print *, "RESULT MATRIX:"
    do i = 1, MATRIX_1_ROW
        print *, (matrixProduct(i,j), j = 1, MATRIX_2_COLUMN)
    end do


    deallocate(matrix1)
    deallocate(matrix2)
    deallocate(matrixProduct)

    print *, "DO IT AGAIN? (Y/N)"
    read (*,*) confirmation

    select case (confirmation)
        case ("Y")
            call input()
        case ("y")
            call input()
        case default
            return
    end select

end subroutine

end module matrix_module

program matrix
    use matrix_module
    implicit none

    write (*,*) "************************************************************************"
    write (*,*) "WELCOME TO THE INTERDIMENSIONAL FORTRAN-90 MATRIX MULTIPLICATION SYSTEM"
    write (*,*) "************************************************************************"

    call input()

end program