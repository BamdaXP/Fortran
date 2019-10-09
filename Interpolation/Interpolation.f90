program Interpolation
	integer :: operation
    operation = 0

    do while(.true.)
        print *,"****************************"
        print *,"Enter the operation you would like to choose to interpolate the function:"
        print *,"The function is : f(x)=1/(1+x**2)"
        print *,"The sampling function is :x(i)=-5+10/N*i"
        print *,"1.Lagrange Interpolation"
        print *,"2.Newton Interpolation"
        print *,"3.Exit the programm"
        read *,operation
        !read the operator from the keyboard
        select case(operation)
			case (1)
				call Lagrange()
			case (2)
				call Newton()
			case (3)
				exit
			case default
				cycle
		end select
	end do

end program

!Lagrange method and its functions
subroutine Lagrange()
	real*8 :: x,P,f

	!Print texts
	print *,"Through sampling, the function is interpolated."
	print *,"The function is shape downbelow with the interval of 0.1 in range [-5,5]"

	!Open files
	open(file="lagrange_y_real.txt",unit=10)
	open(file="lagrange_y.txt",unit=11)
	open(file="lagrange_x.txt",unit=12)

	!Core calculation
	do x=-5,5,0.1
		print "(a,es10.3)","X value:",x
		print "(a,es10.3)","Predicted value:",P(x,15)
		print "(a,es10.3)","Function value:",f(x)
		!Write data into files
		write(10,"(es10.3)")f(x)
		write(11,"(es10.3)")P(x,15)
		write(12,"(es10.3)")x
	end do
end subroutine

function l(x,i,N)
	real*8 :: l,x,x_sample
	integer :: i,N
	l=1
	do j=0,N
		if (j==i) then 
			cycle
		else
			l=l*(x-x_sample(j,N))/(x_sample(i,N)-x_sample(j,N))
		end if
	end do
end function

function P(x,N)
	real*8 :: p,x,x_sample,l
	integer :: N
	P=0
	do i=0,N
		P=P+l(x,i,N)*f(x_sample(i,N))
	end do
end function

!Newton method and its functions
subroutine Newton()

end subroutine



!Function definition
function f(x)
	real*8 :: f
	real*8 :: x
	f=1/(1+x**2)
end function
!Sampling definition
function x_sample(i,N)
	real*8 :: x_sample
	integer :: i,N
	if (i>N) then 
		i=N
	else if (i<0) then
		i=0
	end if 
	x_sample=-5+dble(10)/N*i
end function
