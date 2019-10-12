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
	real*8 :: x,P,f

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
		print "(a,es10.3)","Function value:",f(x)
		!Write data into files
		write(10,"(es10.3)")f(x)
		write(11,"(es10.3)")P(x,15)
		write(12,"(es10.3)")x

	end do

	close(10)
	close(11)
	close(12)
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
	real*8 :: P,x,x_sample,l,f
	integer :: N
	P=0
	do i=0,N
		P=P+l(x,i,N)*f(x_sample(i,N))
	end do
end function

!-------------------------------------------------------------Newton method and its functions--------------------------------
subroutine Newton()
	real*8 :: x,f,Newt

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
		print "(a,es10.3)","Function value:",f(x)
		!Write data into files
		write(10,"(es10.3)")f(x)
		write(11,"(es10.3)")Newt(x,15)
		write(12,"(es10.3)")x
	end do

	close(10)
	close(11)
	close(12)

end subroutine


function Newt(x,N)
	real*8::F_DevDivN,w,x,Newt
	integer :: N
	Newt=0
	do i=0,N
		Newt=Newt+F_DevDivN(i,N)*w(i-1,x,N)
	end do

end function

function F_DevDivN(nn,N)
	real*8 :: F_DevDivN,x_sample,f,w_1
	integer :: nn,N

	F_DevDivN=0
	do i=0,nn
		F_DevDivN=F_DevDivN+f(x_sample(i,N))/w_1(i,x_sample(i,N),N)
	end do

end function

function w(nn,x,N)
	real*8 :: w,x,x_sample
	integer :: N,nn
	w=1
	do i=0,nn
		w=w*(x-x_sample(i,N))
	end do
end function

function w_1(nn,x,N)
	real*8 :: w_1,x,k,x_sample
	integer :: N,nn
	w_1=0
	do i=0,nn
		k=1
		do j=0,nn
			if (.not.i==j) then
				k=k*(x-x_sample(j,N))
			end if
		end do
		w_1=w_1+k
	end do

end function


!-------------------------------------------------------------------------Basic Definition----------------------------
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
