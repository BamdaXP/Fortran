program Sub
    implicit none
    integer :: i

    real*8 :: A(5,5)
    real*8 :: v(5,1)

    real*8 :: AvResult(5,1)
    real*8 :: vAvResult(1,1)
	!Initialize
    A = transpose(reshape((/4,2,2,5,8,2,5,1,3,4,2,1,6,2,6,5,3,2,1,3,8,4,6,3,3/),shape(A)))
    v = reshape((/2,4,5,2,1/),shape(v))
    
    call Calculate(A,v,AvResult,vAvResult)
	
	!Print the final result
    print *,"A matrix:"
    do i=1,5
        print "(5f8.3)",A(i,:)
    end do

    print *,"v vector:"
    do i=1,5
        print "(f8.3)",v(i,:)
    end do

    print *,"AvResult:"
    do i=1,5
        print "(f8.3)",AvResult(i,:)
    end do

    print *,"vAvResult:"
    print "(f8.3)",vAvResult

end program

!Core calculation routine
subroutine Calculate(A, v, AvResult, vAvResult)
	implicit none
	real*8,intent(in) :: A(5,5)
	real*8,intent(in) :: v(5,1)
	real*8,intent(out) :: AvResult(5,1)
	real*8,intent(out) :: vAvResult(1,1)

	AvResult = matmul(A,v)
	vAvResult = matmul(transpose(v),matmul(A,v))

end subroutine