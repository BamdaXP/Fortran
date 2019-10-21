module IO
contains

    subroutine ReadNumFromScreen(number)
        real*8,intent(out) :: number
        !TODO_add_body
        read *,number
    end subroutine ReadNumFromScreen

    subroutine ReadNumsFromFile(filepath,numbers,n)
        integer,intent(in) :: n
        character(len=*),intent(in) :: filepath
        real*8,allocatable,intent(out) :: numbers(:)
        allocate(numbers(n))
        open(file=filepath,unit=10)
        read (10,*)numbers
        close(unit=10)
    end subroutine

    subroutine WriteNumToFile(filepath,number,append)
        character(len=*),intent(in) :: filepath
        real*8,intent(in) :: number
        logical,intent(in) :: append
        if(append)then
            open(file=filepath,unit=10,position="append")
        else
            open(file=filepath,unit=10)
        end if

        write(10,"(es10.3)") number
        close(unit=10)
    end subroutine

    subroutine ReadMatrixFromScreen(matrix,n)
        integer,intent(in) :: n
        real*8,allocatable,intent(out) :: matrix(:,:)

        allocate(matrix(n,n))

        print *,"Please enter 25 numbers to form a 5*5 matrix"
        read *,matrix
        matrix = transpose(matrix)
        print *,"Read matrix complete"
    end subroutine
    
    subroutine ReadMatrixFromFile(filepath,matrix,n)
        character(len=*),intent(in) :: filepath
        integer,intent(in) :: n
        real*8,allocatable,intent(out) :: matrix(:,:)

        allocate(matrix(n,n))

        open(file=filepath,unit=10)
        read (10,*)matrix
        matrix = transpose(matrix)
        close(unit=10)

        print *,"Read matrix complete"
    end subroutine

    subroutine SaveMatrixToFile(filepath,matrix,n)
        character(len=*),intent(in) :: filepath
        integer,intent(in) :: n
        real*8,allocatable,intent(out) :: matrix(:,:)
        allocate(matrix(n,n))

        open(file=filepath,unit=10)
        do i = 1,n
            write(10,"(es10.3)") matrix(i,:)
        end do
        close(unit=10)
        
        print *,"Save matrix complete"
    end subroutine

    subroutine ClearFile(filepath)
        character(len=*),intent(in) :: filepath
        open(file=filepath,unit=10)
        write (10,*)""
        close (unit=10)
    end subroutine ClearFile
end module IO
