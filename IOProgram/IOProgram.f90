program IOProgram
    !Hyper parameter the dim of the matrix
    integer,parameter :: ndim = 5
    real*8 :: matrix(ndim,ndim) = 0
    integer :: operator

    !Main function loop
    do while (.true.)
        print *,"======================="
        print *,"Current buffered matrix"
        !Print buffered matrix
        do i = 1, ndim
            print"(5f8.3)",matrix(i,:)
        end do

        call ReadOperator(operator)
        select case(operator)
            case (1)
                call ReadMatrixFromScreen(matrix)
            case (2)
                call ReadMatrixFromFile(matrix)
            case (3)
                call SaveMatrixToFile(matrix)
            case (4)
                matrix = 0
                 print *,"Reseted the matrix buffer"
            case (5)
                exit
            case default
                print *,"Wrong operation number!"
                cycle
        end select
    end do
    
end program

subroutine ReadOperator(operator)
    integer,intent(out) :: operator

    print *,"Choose the command you would like to use."
    print *,"1.Read a new matrix from screen"
    print *,"2.Read a the matrix from the file"
    print *,"3.Save the buffered matrix into the file"
    print *,"4.Clear the matrix buffer"
    print *,"5.Exit program"
    !Program pauses here
    read *,operator
    
end subroutine

subroutine ReadMatrixFromScreen(matrix)
    real*8,intent(out) :: matrix(5,5)
    print *,"Please enter 25 numbers to form a 5*5 matrix"
    read *,matrix
	matrix = transpose(matrix)
    print *,"Read matrix complete"
end subroutine
 
subroutine ReadMatrixFromFile(matrix)
    real*8,intent(out) :: matrix(5,5)
	
    open(file="./data.txt",unit=10)
    read (10,*)matrix
	matrix = transpose(matrix)
    close(unit=10)

    print *,"Read matrix complete"
end subroutine

subroutine SaveMatrixToFile(matrix)
    real*8,intent(in) :: matrix(5,5)

    open(file="./data.txt",unit=10)
	do i = 1,5
		write(10,"(5f8.3)") matrix(i,:)
	end do
    close(unit=10)
	
	print *,"Save matrix complete"
end subroutine