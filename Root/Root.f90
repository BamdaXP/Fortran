program Root
    real*8 :: x_hat,error
	integer :: iteration

	!Clock to measure
	!Because using int64 so the unit is us(1e-6s)
	integer*8 :: start_time,end_time

	!Procedures for different methods 
	do j = 1,4
		!Start the system timer
		call system_clock(start_time)
		!Loop for different roots
		print *,"================================="
		do i = 1,3
			select case(j)
				case(1)
					call JacobiCalculate(x_hat,error,iteration,i)
					if (i==1) then
						print "(a,/)","#Jacobi Method"
					end if
					
				case(2)
					call NewtonCalculate(x_hat,error,iteration,i)
					
					if (i==1) then
						print "(a,/)","#Newton Method"
					end if
				case(3)
					call AccCalculate(i, x_hat,error,iteration)
					if (i==1) then 
						print "(a,/)","#Post Acceleration Method"
					end if
				case(4)
					call AitkenCalculate(i, x_hat,error,iteration)
					if (i==1) then 
						print "(a,/)","#Aitken Method"
					end if
				case default
					print "(a,/)","Unknown procedure id!!!"
			end select

			!Stop the system timer
			call system_clock(end_time)

			print "(a,i8)","Root id",i
			print "(a,f8.4,/,a,es16.3,/,a,i4,/a,es16.3)","Root:",x_hat,"Error:",error,"Iteration:",iteration
			print "(a,i16,a,/)","Used time:",(end_time-start_time),"us"
		end do
	end do
	
end program Root



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
	x_hat = 0
	last_x = 0
	!Set a large number
	error = 1000000

    do while(abs(error)>1d-8 .and. iteration < 100)
        last_x = x_hat
		
		
		!Choose different functions for different roots
		select case(root_id)
			case (1)
				x_hat = g1(x_hat)
				!Set the x a random number in (-1,0)
				if ( iteration == 0 ) then
					call random_number(x_hat)
					x_hat = x_hat*(-1)
				end if
			case (2)
				x_hat = g2(x_hat)
				!Set the x a random number in (1,2)
				if ( iteration == 0 ) then
					call random_number(x_hat)
					x_hat = x_hat+1
				end if
			case (3)
				x_hat = g2(x_hat)
				!Set the x a random number in (-2,-1)
				if ( iteration == 0 ) then
					call random_number(x_hat)
					x_hat = x_hat*(-1)-1
				end if
		end select

		error = abs(x_hat-last_x)
        iteration = iteration + 1
    end do
    
end subroutine JacobiCalculate

subroutine NewtonCalculate(x_hat,error,iteration,root_id)
	integer,intent(in) :: root_id
	real*8,intent(out) ::x_hat,error
	integer,intent(out) :: iteration
	
	real*8 :: f,f_derivative

	real*8 :: last_x


	!Initialize the x with a random number in different regions
	select case(root_id)
			case (1)
				!Set the x a random number in (0,1)
				call random_number(x_hat)
			case (2)
				!Set the x a random number in (1,2)
				call random_number(x_hat)
				x_hat = x_hat+1
			case (3)
				!Set the x a random number in (-2,-1)
				call random_number(x_hat)
				x_hat = (x_hat+1)*(-1)
	end select
	iteration = 0
	!Set a large number
	error = 10000000
	
	!Main calculation loop
	do while(abs(error)>1d-8 .and. iteration < 100)
		last_x = x_hat
		x_hat = x_hat-f(x_hat)/f_derivative(x_hat)
		error = abs(last_x - x_hat)
		iteration = iteration + 1
	end do

end subroutine

!Post Acceleration Method
subroutine AccCalculate(root_id, x_hat,error,iteration)
	implicit none
	integer,intent(in) :: root_id
	real*8,intent(out) :: x_hat,error
	integer,intent(out) :: iteration

	real*8 :: g,g1,g2,g1_derivative,g2_derivative,l
	real*8 :: last_x

	!Initialize the x with a random number in different regions
	!Initialize the factor l
	select case(root_id)
			case (1)
				!Set the x a random number in (0,1)
				call random_number(x_hat)
				l = g1_derivative(x_hat)
			case (2)
				!Set the x a random number in (1,2)
				call random_number(x_hat)
				x_hat = x_hat+1
				l = g2_derivative(x_hat)
			case (3)
				!Set the x a random number in (-2,-1)
				call random_number(x_hat)
				x_hat = x_hat*(-1)-1
				l = g2_derivative(x_hat)
	end select

	iteration = 0
	!Set a large number
	error = 10000000

	do while(abs(error)>1d-8 .and. iteration < 100)
		last_x = x_hat
		select case(root_id)
			case(1)
				g = g1(x_hat)
			case default
				g = g2(x_hat)
		end select
		x_hat = (1-l)**(-1)*(g-l*x_hat)
		error = abs(last_x - x_hat)
		iteration = iteration + 1
	end do
end subroutine AccCalculate

!Aitken Calculation
subroutine AitkenCalculate(root_id, x_hat,error,iteration)
	implicit none
	integer,intent(in) :: root_id
	real*8,intent(out) :: x_hat,error
	integer,intent(out) :: iteration

	real*8 :: g,g1,g2,g1_derivative,g2_derivative,l
	real*8 :: last_x,last_x_buffer,x_2

	!Initialize the x with a random number in different regions
	!Initialize the factor l
	select case(root_id)
			case (1)
				!Set the x a random number in (0,1)
				call random_number(x_hat)
				l = g1_derivative(x_hat)
			case (2)
				!Set the x a random number in (1,2)
				call random_number(x_hat)
				x_hat = x_hat+1
				l = g2_derivative(x_hat)
			case (3)
				!Set the x a random number in (-2,-1)
				call random_number(x_hat)
				x_hat = x_hat*(-1)-1
				l = g2_derivative(x_hat)
	end select

	select case(root_id)
		case(1)
			last_x = g1(x_hat)
			x_hat = g1(last_x)
		case default
			last_x = g2(x_hat)
			x_hat = g2(last_x)
	end select
	iteration = 0
	!Set a large number
	error = 10000000



	!Main loop 
	do while(abs(error)>1d-8 .and. iteration < 100)

		select case(root_id)
		case(1)
			x_2 = g1(x_hat)
		case default
			x_2 = g2(x_hat)
		end select
		last_x_buffer = x_hat
		x_hat = x_2 - (x_2-x_hat)**2/(x_2-2*x_hat+last_x)
		error = abs(last_x - x_hat)
		last_x = last_x_buffer
		iteration = iteration + 1
	end do

end subroutine AitkenCalculate





!Functions definitions
function f(x)
	implicit none
	real*8 :: x
	real*8 :: f
	f = x**3/3-x
end function

function f_derivative(x)
	implicit none
	real*8 :: x
	real*8 :: f_derivative
	f_derivative = x**2-1
end function	
!g1 = x**3/3
function g1(x)
    implicit none
    real*8 :: x
    real*8 :: g1
    g1 = x**3/3
end function
function g1_derivative(x)
	implicit none
	real*8 :: x
	real*8 :: g1_derivative
	g1_derivative = x**2
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
!g2 = x**(1.0/3)/3
function g2_derivative(x)
    implicit none
    real*8 :: x
    real*8 :: g2_derivative
    if ( x>0 ) then
        g2_derivative = (3*x)**(dble(-2.0)/dble(3.0))
    else
        g2_derivative = sign(abs(3*x)**(dble(-2.0)/dble(3.0)),x)
    end if
end function