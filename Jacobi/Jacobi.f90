program Jacobi
    implicit none
    real*8 :: x_hat,error
    integer :: iteration

    call JacobiCalculate(x_hat,error,iteration)
    print "(a,f8.4,/,a,f8.8,/,a,i4)","Root:",x_hat,"    Error:",error,"     Iteration:",iteration
end program Jacobi

function g1(x)
    implicit none
    real*8 :: x
    real*8 :: g1
    g1 = x**3/3
end function

function g2(x)
    implicit none
    real*8 :: x
    real*8 :: g2
    if ( x>0 ) then
        g2 = x**(1.0/3)/3
    else
        g2 = sign(abs(x)**(1.0/3.0),x)
    end if
    
end function

subroutine JacobiCalculate(x_hat,error,iteration)
    implicit none
    real*8 :: g2
    real*8,intent(out) :: x_hat,error
    integer,intent(out) :: iteration

    real*8 :: last_x

    iteration = 0

    do while(abs(x_hat-last_x)<0.0001 .and. iteration < 100)
        last_x = x_hat
        if ( iteration == 0 ) then
            call random_number(x_hat)
            x_hat = x_hat*2
        end if
        x_hat = g2(x_hat)
        iteration = iteration + 1
    end do
    error = abs(x_hat-last_x)
end subroutine JacobiCalculate