program Jacobi
    real*8 :: x_hat,error
    integer :: iteration
	do i = 1,3
		call JacobiCalculate(x_hat,error,iteration,i)
		print "(a,i)","Root id",i
		print "(a,f8.4,/,a,f,/,a,i4)","Root:",x_hat,"Error:",error,"Iteration:",iteration
	end do
end program Jacobi

!g1 = x**3/3
function g1(x)
    implicit none
    real*8 :: x
    real*8 :: g1
    g1 = x**3/3
end function

!g2 = x**(1.0/3)/3
function g2(x)
    implicit none
    real*8 :: x
    real*8 :: g2
    if ( x>0 ) then
        g2 = (3*x)**(1.0/3)
    else
        g2 = sign(abs(3*x)**(1.0/3.0),x)
    end if
    
end function

!The core calculation subroutine
subroutine JacobiCalculate(x_hat,error,iteration,root_id)
    implicit none
    real*8 :: g1
	real*8 :: g2
	integer,intent(in):: root_id
    real*8,intent(out) :: x_hat,error
    integer,intent(out) :: iteration

    real*8 :: last_x

    iteration = 0
	x_hat =0 
	!Set a large number
	last_x = 10000000

    do while(abs(x_hat-last_x)>0.00001 .and. iteration < 100)
        last_x = x_hat
		
		
		!Choose different functions for different roots
		select case(root_id)
			case (1)
				x_hat = g1(x_hat)
				!Set the x a random number in (0,2)
				if ( iteration == 0 ) then
					call random_number(x_hat)
					x_hat = x_hat*2
				end if
			case (2)
				x_hat = g2(x_hat)
				!Set the x a random number in (0,2)
				if ( iteration == 0 ) then
					call random_number(x_hat)
					x_hat = x_hat*2
				end if
			case (3)
				x_hat = g2(x_hat)
				!Set the x a random number in (-2,0)
				if ( iteration == 0 ) then
					call random_number(x_hat)
					x_hat = x_hat*-2
				end if
		end select

        iteration = iteration + 1
    end do
    error = abs(x_hat-last_x)
end subroutine JacobiCalculate