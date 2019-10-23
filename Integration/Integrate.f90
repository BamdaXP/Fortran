module Integrate
    abstract interface
        function func(x)
            real*8,intent(in) :: x
            real*8 :: func
        end function func
    end interface
contains
    subroutine Simpson(f_ptr, x_start,x_end,step,result)
        procedure(func),pointer,intent(in) ::  f_ptr
        real*8,intent(in)  :: x_start,x_end,step
        real*8,intent(out) :: result
        !Local vars
        real*8 :: x,h

        result = 0

        x=x_start
        h=step

        do while(.true.)
            if(x+h>x_end)then
                h=x_end-x
            end if

            result=result+h/6*(f_ptr(x)+4*f_ptr(x+h/2)+f_ptr(x+h))
            x=x+h

            if (x>=x_end)then
                exit
            end if
        end do
        !TODO_add_body
    end subroutine Simpson



    subroutine Trapezoid (f_ptr, x_start,x_end,step,result)
        procedure(func),pointer,intent(in) ::  f_ptr
        real*8,intent(in)  :: x_start,x_end,step
        real*8,intent(out) :: result
        !Local vars
        real*8 :: x,h

        result = 0

        x=x_start
        h=step

        do while(.true.)
            if(x+h>x_end)then
                h=x_end-x
            end if

            result=result+(f_ptr(x)+f_ptr(x+h))*h/2
            x=x+h

            if (x>=x_end)then
                exit
            end if
        end do
    end subroutine Trapezoid 

end module Integrate