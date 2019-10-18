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

!-------------------------------------------------Lagrange method and its functions-------------------------------------------
subroutine Lagrange()
	real*8 :: x,P,y

	!Print texts
	print *,"Through sampling, the function is interpolated by Lagrange Interpolation."
	print *,"The function is shape downbelow with the interval of 0.1 in range [-5,5]"

	!Open files
	open(file="lagrange_y_real.txt",unit=10)
	open(file="lagrange_y.txt",unit=11)
	open(file="lagrange_x.txt",unit=12)

	!Core calculation
	do x=-5,5,0.1
		print *,"-----"
		print "(a,es10.3)","X value:",x
		print "(a,es10.3)","Predicted value:",P(x,15)
		print "(a,es10.3)","Function value:",y(x)
		!Write data into files
		write(10,"(es10.3)")y(x)
		write(11,"(es10.3)")P(x,15)
		write(12,"(es10.3)")x

	end do

	close(10)
	close(11)
	close(12)
end subroutine

function P(x,N)
	!Input
	real*8,intent(in) :: x
	integer,intent(in) :: N
	!Output
	real*8 :: P
	!Used functions
	real*8 :: x_sample,y
	!Local vars
	real*8 :: x_samples(16),y_samples(16),mult
	
	
	

	!Calculate the samples
	do i=0,N
		x_samples(i)=x_sample(i,N)
		y_samples(i)=y(x_samples(i))
	end do

	!Main calculation
	P=0
	do k=0,N
		mult=1.0
		do j=0,N
			if (.not.j==k)then
				mult=mult*(x-x_samples(j))/(x_samples(k)-x_samples(j))
			end if
		end do
		P=P+mult*y_samples(k)
	end do
end function

!-------------------------------------------------------------Newton method and its functions--------------------------------
subroutine Newton()
	real*8 :: x,y,Newt

	!Print texts
	print *,"Through sampling, the function is interpolated by Newton Interpolation."
	print *,"The function is shape downbelow with the interval of 0.1 in range [-5,5]"

	!Open files
	open(file="newton_y_real.txt",unit=10)
	open(file="newton_y.txt",unit=11)
	open(file="newton_x.txt",unit=12)

	!Core calculation
	do x=-5,5,0.1
		print *,"-----"
		print "(a,es10.3)","X value:",x
		print "(a,es10.3)","Predicted value:",Newt(x,15)
		print "(a,es10.3)","Function value:",y(x)
		!Write data into files
		write(10,"(es10.3)")y(x)
		write(11,"(es10.3)")Newt(x,15)
		write(12,"(es10.3)")x
	end do

	close(10)
	close(11)
	close(12)

end subroutine

function Newt2(x,N)
	!Input
	real*8,intent(in) :: x
	integer,intent(in) :: N
	!Output
	real*8 :: Newt
	!Used functions
	real*8 :: x_sample,y
	!Local vars
	real*8 :: d(15,15),x_samples(16),y_samples(16),a(16),b(16),c(16)

	!Calculate the samples
	do i=0,N
		x_samples(i)=x_sample(i,N)
		y_samples(i)=y(x_samples(i))
	end do


	a(1)=y_samples(1)
	
	do k=0,N-1
		d(k,1)=(y_samples(k+1)-y_samples(k))/(x_samples(k+1)-x_samples(k))
	end do

	do j=1,N-1
		do k=0,N-j
			d(k,j)=(d(k+1,j-1)-d(k,j-1))/(x_samples(k+j)-x_samples(k))
		end do
	end do

	do j=1,N
		a(j)=d(0,j-1)
	end do

	b(0)=1
	c(0)=a(0)

	do j=1,N
		b(j)=(x-x_samples(j-1))*b(j-1)
		c(j)=a(j)*b(j)
	end do

	Newt=sum(c)
end function

function Newt(x,N)
	!Input
	real*8,intent(in) :: x
	integer,intent(in) :: N
	!Output
	real*8 :: Newt
	!Used functions
	real*8 :: x_sample,y
	!Local vars
	real*8 :: d(16,16),x_samples(16),y_samples(16),a,b

	!Calculate the samples
	do i=0,N
		x_samples(i)=x_sample(i,N)
		y_samples(i)=y(x_samples(i))
	end do


	do i=0,N
		d(i,0)=y_samples(i)
	end do
	do i=1,N
		do j=1,i
			d(i,j)=(d(i,j-1)-d(i-1,j-1))/(x_samples(i)-x_samples(i-j))
		end do
	end do


	a=0
	b=1
	do i=0,N
		a=a+b*d(i,i)
		b=b*(x-x_samples(i))
	end do

	Newt=a

end function

!-------------------------------------------------------------Spline method and its functions--------------------------------
subroutine Spline(x,N)
	!Input
	real*8,intent(in) :: x
	integer,intent(in) :: N
	!Output
	real*8 :: Newt
	!Used functions
	real*8 :: x_sample,y,y_1
	!Local vars
	real*8 :: x_samples(16),y_samples(16),h(15),a(15),b(15),c(15,1),D(15,15)

	do i=0,14
		h(i)=x_samples(i+1)-x_samples(i)
	end do
	
	do i=0,14
		a(i)=h(i)/(h(i)+h(i+1))
	end do
	do i=0,14
		if (i==0)then 
			b(i)=3*y_1(-5)
		else if(i==14)then
			b(i)=3*y_1(5)
		else
			b(i)=3*(1-a(i))*((y_samples(i)-y_samples(i-1))/h(i-1))+	(a(i)*(y_samples(i+1)-y_samples(i))/h(i))
		end if

	end do
	!setup D matrix
	do i=0,14
		D(i,i)=2
		if(i>0) then
			D(i,i-1)=1-a(i)
		end if
		if(i<14) then 
			D(i,i+1)=a(i)
		end if 
	end do
		
	c = reshape( b, (/ 15, 1 /) )
	call GaussElimination(D,b)

	
end subroutine
subroutine GaussElimination(A, b)
    real*8 :: A(15,15),b(15,1)
    real*8 :: factor,A_temp(15,15)

    !Creating copies of parameters in case of reference affecting
    A_temp = A

    do i=1,15
        !Cast the diag elements to unit 1
        factor = A_temp(i,i)
        do j=1,15
            A_temp(i,j) = A_temp(i,j)/factor
        end do
        b(i,1) = b(i,1)/factor

        !Eliminate bottom triangle
        do j = i+1,15
            factor = A_temp(j,i)
            do k = i,15
                A_temp(j,k) = A_temp(j,k) - factor*A_temp(i,k)
            end do
            b(j,1) = b(j,1) - factor*b(i,1)
        end do

    end do

    !Eliminate upper triangle
    do i=1,15
        do j=i+1,15
            factor = A_temp(16-j,16-i)
            do k = 16-i,15
                A_temp(16-j,k) = A_temp(16-j,k) - factor*A_temp(16-i,k)
            end do
            b(16-j,1) = b(16-j,1) - factor*b(16-i,1)
        end do
	end do
end subroutine
!-------------------------------------------------------------------------Basic Definition----------------------------
!Function definition
function y(x)
	real*8 :: y
	real*8,intent(in) :: x
	y=1/(1+x**2)
end function
function y_1(x)
	real*8 :: y_1
	real*8,intent(in) :: x
	y=-2*x/(1+x**2)**2
end function
!Sampling definition
function x_sample(i,N)
	real*8 :: x_sample
	integer,intent(in) :: i,N
	integer :: j
	if (i>N) then 
		j=N
	else if (i<0) then
		j=0
	else
		j=i
	end if 
	x_sample=-5+dble(10)/N*j
end function
